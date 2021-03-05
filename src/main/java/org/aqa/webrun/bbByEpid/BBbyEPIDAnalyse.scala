package org.aqa.webrun.bbByEpid

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import org.aqa.AngleType
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.db.BBbyEPID
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.web.ViewOutput
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.dailyQA.DailyQAActivity
import org.aqa.webrun.dailyQA.DailyQACSVCacheComposite
import org.restlet.Response

import java.text.SimpleDateFormat
import java.util.Date
import javax.vecmath.Point3d

/**
 * Given validated data, process it.
 */
object BBbyEPIDAnalyse extends Logging {

  private def isVert(al: AttributeList) = AngleType.isAngleType(Util.gantryAngle(al), AngleType.vertical)

  private val ls = System.lineSeparator
  private val lsn = "@@n@@"

  /**
   * Create a Matlab script that shows calculations.
   *
   * @param epid         EPID DICOM
   * @param bbLocation   in EPID translated to mm
   * @param extendedData Associated DB rows
   */
  private def constructEpidMatlab(result: BBbyEPIDImageAnalysis.Result): String = {

    def getDbls(tag: AttributeTag) = result.al.get(tag).getDoubleValues

    val gantryAngle = getDbls(TagByName.GantryAngle).head
    val name = if (isVert(result.al)) "Vert" else "Horz"
    val ipps = getDbls(TagByName.ImagePlanePixelSpacing)
    val rtip = getDbls(TagByName.RTImagePosition)
    val rTrans = getDbls(TagByName.XRayImageReceptorTranslation)
    val gaRounded = Util.angleRoundedTo90(Util.gantryAngle(result.al))
    val sinCos = if (isVert(result.al))
      "epidIsoX = cos(deg2rad(gantryAngleVert)) * (epidIsoVertX + VertTX);"
    else
      "epidIsoY = sin(deg2rad(gantryAngleHorz)) * (epidIsoHorzX + HorzTX);"

    val fprintfVert =
      s"""
fprintf("MV G ${gaRounded.formatted("%3d")} (BB - DIGITAL_CAX) @ ISOCENTER PLANE:  %f   NA  %f  %f@@n@@",  epidIsoX, epidVertZ, sqrt(epidIsoX*epidIsoX + epidVertZ*epidVertZ));
fprintf("MV G ${gaRounded.formatted("%3d")} (BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f   NA   %f   %f$lsn", epidIsoX - cbctX, epidVertZ - cbctZ, sqrt((epidIsoX - cbctX)*(epidIsoX - cbctX) + (epidVertZ - cbctZ)*(epidVertZ - cbctZ)));
"""

    val fprintfHorz =
      s"""
fprintf("MV G ${gaRounded.formatted("%3d")} (BB - DIGITAL_CAX) @ ISOCENTER PLANE:  NA   %f  %f  %f$lsn", epidIsoY, epidHorzZ, sqrt(epidIsoY*epidIsoY + epidHorzZ*epidHorzZ));
fprintf("MV G ${gaRounded.formatted("%3d")} (BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  NA   %f   %f   %f$lsn", epidIsoY - cbctY, epidHorzZ - cbctZ, sqrt((epidIsoY - cbctY)*(epidIsoY - cbctY) + (epidHorzZ - cbctZ)*(epidHorzZ - cbctZ)));
"""

    val fprintf = if (isVert(result.al)) fprintfVert else fprintfHorz

    val text =
      s"""
%% Perform isoplane projection and map to RTPLAN coordinates for beam $name : ${Util.angleRoundedTo90(gantryAngle)}

RTImageSID$name = ${getDbls(TagByName.RTImageSID).head};     %% From DICOM.  Distance from beam to image plane in mm

RadiationMachineSAD$name = ${getDbls(TagByName.RadiationMachineSAD).head};     %% From DICOM.  Distance from beam to isoplane in mm

ImagePlanePixelSpacing${name}X = ${ipps(0)};     %% From DICOM.  Width  of pixel in mm in image plane
ImagePlanePixelSpacing${name}Y = ${ipps(1)};     %% From DICOM.  Height of pixel in mm in image plane

RTImagePosition${name}X = ${rtip(0)};     %% From DICOM.  X Coordinate of center of leftmost pixel in mm in image plane.
RTImagePosition${name}Y = ${rtip(1)};     %% From DICOM.  Y Coordinate of center of topmost pixel in mm in image plane.

%% Coordinates in pixels in the image plane where the bb was found by AQA software.  (0,0 is upper left corner of image)
epidPix${name}X = ${result.pix.getX};
epidPix${name}Y = ${result.pix.getY};

divergence$name = RTImageSID$name / RadiationMachineSAD${name};   %% Beam divergence factor.  Is usually close to 1.5

%% The coordinates of the bb in the isoplane in mm, with origin in the center of the image, and X positive direction is
%% to the right.  Y positive direction is up, which is the opposite direction of the pixel coordinate system.
epidIso${name}X = ((epidPix${name}X * ImagePlanePixelSpacing${name}X) + RTImagePosition${name}X) / divergence${name};
epidIso${name}Y = ((epidPix${name}Y * ImagePlanePixelSpacing${name}Y * -1) + RTImagePosition${name}Y) / divergence${name};

%% XRayImageReceptorTranslation $name values scaled from mm in the image plane to mm in the isoplane.
${name}TX = ${rTrans(0)} / divergence${name};
${name}TY = ${rTrans(1)} / divergence${name};

%% From DICOM.  Gantry angle in degrees.
gantryAngle${name} = ${gantryAngle};

%% Convert the X value in the image from gantry coordinates to RTPLAN (world) coordinates.  For vertical gantry angles, X maps to X, but for horizontal gantry angles, X maps to Y.
${sinCos}

%% Convert the Y value in the image to RTPLAN (world) coordinates.  Y always maps to Z, But the sign must be flipped (negated).
epid${name}Z = epidIso${name}Y + ${name}TY;

%% Print the results in a format similar to the web report.
${fprintf}
"""

    text
  }

  private def constructCompositeMatlab(epidResultList: Seq[BBbyEPIDImageAnalysis.Result], bbByCBCT: BBbyCBCT, response: Response): String = {
    val separator = ls + ls + "%% ------------------------------------------------------------------" + ls + ls

    val header = {
      s"""
%% Calculate EPID results with Matlab code.  This code may be run with Matlab.  This code is provided as a convenience to
%% allow users to ascertain that calculations are being done correctly.
%%
%% Note that the AQA generates these values using entirely different code (written in Scala), but because the answers match,
%% users can be assured that the calculations are equivalent.  The only difference might be in roundoff errors, but the 
%% results will match to at least 10 significant figures.

%% EPID report: ${response.getRequest.getHostRef}${ViewOutput.viewOutputUrl(epidResultList.head.bbByEpid.outputPK)}

%% Results from CBCT
cbctX = ${bbByCBCT.cbctX_mm} - ${bbByCBCT.rtplanX_mm};
cbctY = ${bbByCBCT.cbctY_mm} - ${bbByCBCT.rtplanY_mm};
cbctZ = ${bbByCBCT.cbctZ_mm} - ${bbByCBCT.rtplanZ_mm};
fprintf("CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f  %f  %f$lsn", cbctX, cbctY, cbctZ);
"""
    }

    val epidText = epidResultList.map(er => constructEpidMatlab(er))

    val compositeText = {
      s"""
%% Calculate the Z position of the bb in RTPLAN (world) coordinates by averaging the two values from the vertical and horizontal images."
epidIsoZ = (epidVertZ + epidHorzZ) / 2.0;

%% Subtract the CBCT position from the EPID position to find the distance between them.
epidMinuscbctX = epidIsoX - cbctX;
epidMinuscbctY = epidIsoY - cbctY;
epidMinuscbctZ = epidIsoZ - cbctZ;
%% Format final values similarly to web page report.
fprintf("AVERAGE MV(BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f   %f  %f  %f$lsn", epidIsoX - cbctX, epidIsoY - cbctY, epidIsoZ - cbctZ, sqrt((epidIsoX - cbctX)*(epidIsoX - cbctX) + (epidIsoY - cbctY)*(epidIsoY - cbctY) + (epidIsoZ - cbctZ)*(epidIsoZ - cbctZ)));"""
    }

    val allText = (Seq(header) ++ epidText ++ Seq(compositeText)).mkString(separator)

    allText.replaceAll(lsn, "\\\\n")
  }

  private def constructComposite(epidResultList: Seq[BBbyEPIDImageAnalysis.Result], extendedData: ExtendedData, runReq: BBbyEPIDRunReq, response: Response): Either[String, (BBbyEPIDComposite, Option[String])] = {

    // val bbByEPIDList = epidResultList.map(er => er._1)

    def getByAngleType(angleType: AngleType.Value) = {
      val at = angleType.toString

      def sameType(bbByEPID: BBbyEPID): Boolean = {
        val angTyp = AngleType.classifyAngle(bbByEPID.gantryAngle_deg)
        angTyp.isDefined && angTyp.get.toString.equals(at)
      }

      epidResultList.filter(r => sameType(r.bbByEpid))
    }

    val vert = getByAngleType(AngleType.vertical)
    val horz = getByAngleType(AngleType.horizontal)

    if ((vert.nonEmpty && horz.nonEmpty)) {
      // Use the same number of vertical and horizontal beams to get the averages.  Handles cases where are there are many of one and few of the others.
      val max = Math.min(vert.size, horz.size) // maximum number of images to use in each of the vertical and horizontal directions.
      val x_mm = vert.take(max).map(r => r.bbByEpid.epid3DX_mm).sum / max
      val y_mm = horz.take(max).map(r => r.bbByEpid.epid3DY_mm).sum / max
      val z_mm = epidResultList.map(r => r.bbByEpid.epid3DZ_mm).sum / epidResultList.size
      val offset_mm = (new Point3d(x_mm, y_mm, z_mm)).distance(new Point3d(0, 0, 0))

      val SeriesInstanceUID = runReq.epidList.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

      val bbByCBCTHistory: Option[BBbyCBCT.BBbyCBCTHistory] = {
        Procedure.ProcOfBBbyCBCT match {
          case Some(proc) => {
            val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
            val epidDateTime = extendedData.output.dataDate.get
            val epidDateFormatted = dateFormat.format(epidDateTime)
            val list = BBbyCBCT.history(extendedData.machine.machinePK.get, proc.procedurePK.get)

            /**
             * CBCT must have been acquired before EPID, and must be on the same date.
             */
            def qualifies(date: Date): Boolean = {
              (epidDateTime.getTime >= date.getTime) && dateFormat.format(date).equals(epidDateFormatted)
            }

            list.filter(c => qualifies(c.date)).sortBy(c => c.date.getTime).lastOption
          }
          // BBbyCBCT.getProcedurePK not defined,  Must be that sthere are no BBbyCBCT rows.
          case _ => None
        }
      }

      val rtplanSopInstanceUID: Option[String] = {
        if (bbByCBCTHistory.isDefined) {
          Some(bbByCBCTHistory.get.bbByCBCT.rtplanSOPInstanceUID)
        } else None
      }

      val bbByEPIDComposite = new BBbyEPIDComposite(
        bbByEPIDCompositePK = None,
        outputPK = extendedData.output.outputPK.get,
        rtplanSOPInstanceUID = rtplanSopInstanceUID,
        epidSeriesInstanceUID = SeriesInstanceUID,
        offset_mm,
        x_mm,
        y_mm,
        z_mm,
        None,
        None,
        None, None, None,
        None, None, None)

      val matlabComposite = if (bbByCBCTHistory.isDefined) Some(constructCompositeMatlab(epidResultList, bbByCBCTHistory.get.bbByCBCT, response)) else None

      // if a corresponding CBCT is defined, then incorporate those values.
      val bbByEPIDCompositeFinal = {
        if (bbByCBCTHistory.isDefined) {
          val c = bbByCBCTHistory.get.bbByCBCT
          val x = x_mm - c.err_mm.getX
          val y = y_mm - c.err_mm.getY
          val z = z_mm - c.err_mm.getZ
          val offset = new Point3d(0, 0, 0).distance(new Point3d(x, y, z))
          bbByEPIDComposite.copy(
            bbByCBCTPK = bbByCBCTHistory.get.bbByCBCT.bbByCBCTPK,
            offsetAdjusted_mm = Some(offset),
            xAdjusted_mm = Some(x),
            yAdjusted_mm = Some(y),
            zAdjusted_mm = Some(z),
            tableXlateral_mm = Some(epidResultList.head.bbByEpid.tableXlateral_mm - c.tableXlateral_mm),
            tableYvertical_mm = Some(epidResultList.head.bbByEpid.tableYvertical_mm - c.tableYvertical_mm),
            tableZlongitudinal_mm = Some(epidResultList.head.bbByEpid.tableZlongitudinal_mm - c.tableZlongitudinal_mm))
        } else bbByEPIDComposite
      }

      Right((bbByEPIDCompositeFinal, matlabComposite))
    } else {
      if (vert.isEmpty)
        Left("No images with BB with vertical gantry angle.")
      else
        Left("No images with BB with horizontal gantry angle.")
    }
  }

  def runProcedure(extendedData: ExtendedData, runReq: BBbyEPIDRunReq, response: Response): ProcedureStatus.Value = {
    try {
      logger.info("Starting analysis of EPID Alignment for machine " + extendedData.machine.id)

      val bbLocList = runReq.epidList.par.map(epid => BBbyEPIDImageAnalysis.findBB(epid, extendedData.output.outputPK.get)).toList

      //  bbLocList.map(bbl => if (bbl.isLeft) Left(bbl.left.get) else (

      val successList = bbLocList.filter(_.isRight).map(r => r.right.get) // list of bb's that were successfully found.
      logger.info("Inserting " + successList.size + " BBbyEPID records into database")
      BBbyEPID.insertSeq(successList.map(_.bbByEpid))

      logger.info("Calculating composite result.")
      val bbByEPIDComposite = constructComposite(successList, extendedData, runReq, response)
      if (bbByEPIDComposite.isRight) {
        logger.info("Inserting composite EPID record into database: " + bbByEPIDComposite.right.get)
        bbByEPIDComposite.right.get._1.insert
      } else
        logger.info("No composite EPID record created.  Reported error: " + bbByEPIDComposite.left.get)

      DailyQAActivity.update() // tell web page that data has changed
      val procedureStatus = if (bbByEPIDComposite.isRight) ProcedureStatus.done else ProcedureStatus.fail

      BBbyEPIDHTML.generateHtml(extendedData, bbLocList, bbByEPIDComposite, runReq, ProcedureStatus.done)
      logger.info("Finished analysis of EPID Alignment for machine " + extendedData.machine.id)
      ProcedureStatus.done
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of BBbyEPIDAnalyse" + ": " + t + fmtEx(t))
        ProcedureStatus.crash
      }
    }
  }

}
