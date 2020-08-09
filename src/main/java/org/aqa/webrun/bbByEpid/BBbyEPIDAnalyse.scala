package org.aqa.webrun.bbByEpid

import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import javax.vecmath.Point2d
import org.aqa.db.BBbyEPID
import javax.vecmath.Point3d
import org.aqa.run.ProcedureStatus
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.BBbyCBCT
import java.util.Date
import java.text.SimpleDateFormat
import gnu.crypto.mode.CBC
import org.aqa.AngleType
import com.pixelmed.dicom.AttributeTag

/**
 * Given validated data, process it.
 */
object BBbyEPIDAnalyse extends Logging {

  /**
   * Translate BB position in ISO plane to RTPLAN coordinates
   *
   * @param epid EPID DICOM
   *
   * @param bbLocation in EPID translated to mm
   *
   * @param extendedData Associated DB rows
   */
  private def toBBbyEPID(epid: AttributeList, bbLocation: Point2d, extendedData: ExtendedData): BBbyEPID = {
    val gantryAngle_deg = Util.gantryAngle(epid)
    val gantryAngle_rad = Math.toRadians(gantryAngle_deg)

    val epidOffset: Point3d = {
      val at = epid.get(TagFromName.XRayImageReceptorTranslation)
      if (at == null) new Point3d(0, 0, 0)
      else {
        val trans = at.getDoubleValues
        new Point3d(trans(0), trans(1), trans(2))
      }
    }

    val epid3DX_mm = Math.cos(gantryAngle_rad) * (bbLocation.getX - epidOffset.getX)
    val epid3DY_mm = Math.sin(gantryAngle_rad) * (bbLocation.getX - epidOffset.getX)
    val epid3DZ_mm = (-bbLocation.getY) - epidOffset.getY // flip sign to directionally match coordinate system
    val origin = new Point3d(0, 0, 0)

    def getDbl(tag: AttributeTag) = epid.get(tag).getDoubleValues.head

    val bbByEPID = new BBbyEPID(
      bbByEPIDPK = None,
      outputPK = extendedData.output.outputPK.get,
      // rtplanSOPInstanceUID = rtplanSOP,
      epidSOPInstanceUid = Util.sopOfAl(epid),
      offset_mm = (new Point3d(epid3DX_mm, epid3DY_mm, epid3DZ_mm)).distance(origin),
      gantryAngle_deg = gantryAngle_deg,
      status = ProcedureStatus.done.toString,
      epidImageX_mm = bbLocation.getX,
      epidImageY_mm = bbLocation.getY,
      epid3DX_mm, epid3DY_mm, epid3DZ_mm,
      getDbl(TagFromName.TableTopLateralPosition), // tableXlateral_mm
      getDbl(TagFromName.TableTopVerticalPosition), // tableYvertical_mm
      getDbl(TagFromName.TableTopLongitudinalPosition)) // tableZlongitudinal_mm

    bbByEPID
  }

  private def isVert(al: AttributeList) = AngleType.isAngleType(Util.gantryAngle(al), AngleType.vertical)

  private val ls = System.lineSeparator

  /**
   * Create a Matlab script that shows calculations.
   *
   * @param epid EPID DICOM
   *
   * @param bbLocation in EPID translated to mm
   *
   * @param extendedData Associated DB rows
   */
  private def constructEpidMatlab(result: BBbyEPIDImageAnalysis.Result): String = {

    def getDbls(tag: AttributeTag) = result.al.get(tag).getDoubleValues

    val name = if (isVert(result.al)) "Vert" else "Horz"

    val epidComment = "%% Perform isoplane projection and map to RTPLAN coordinates for beam " + name

    val RTImageSID = "RTImageSID" + name + " = " + getDbls(TagFromName.RTImageSID).head + ";"

    val RadiationMachineSAD = "RadiationMachineSAD" + name + " = " + getDbls(TagFromName.RadiationMachineSAD).head + ";"

    val ImagePlanePixelSpacing = {
      val ipps = getDbls(TagFromName.ImagePlanePixelSpacing)
      val x = "ImagePlanePixelSpacing" + name + "X = " + ipps(0) + ";"
      val y = "ImagePlanePixelSpacing" + name + "Y = " + ipps(1) + ";"
      x + ls + y
    }

    val RTImagePosition = {
      val rtip = getDbls(TagFromName.RTImagePosition)
      val x = "RTImagePosition" + name + "X = " + rtip(0) + ";"
      val y = "RTImagePosition" + name + "Y = " + rtip(1) + ";"
      x + ls + y
    }

    val pix = {
      val comment = "%% Coordinates in pixels where the bb was found."
      val x = "epidPix" + name + "X = " + result.pix.getX + ";"
      val y = "epidPix" + name + "Y = " + result.pix.getY + ";"
      comment + ls + x + ls + y
    }

    val divergence = "divergence" + name + " = RTImageSID" + name + " / RadiationMachineSAD" + name + ";"

    val topLeft = {
      val comment = "%% The center of the image's top left corner pixel in isoplane coordinates in mm."
      val x = "TopLeft" + name + "X = RTImagePosition" + name + "X / divergence" + name + ";"
      val y = "TopLeft" + name + "Y = -RTImagePosition" + name + "Y / divergence" + name + ";"
      comment + ls + x + ls + y
    }

    val iso = {
      val comment = "%% The coordinates of the bb in the isoplane in mm."
      val x = "epidIso" + name + "X = ((" + "epidPix" + name + "X * ImagePlanePixelSpacing" + name + "X) / divergence" + name + ")" + " + " + "TopLeft" + name + "X;"
      val y = "epidIso" + name + "Y = ((" + "epidPix" + name + "Y * ImagePlanePixelSpacing" + name + "Y) / divergence" + name + ")" + " + " + "TopLeft" + name + "Y;"
      comment + ls + x + ls + y
    }

    val XRayImageReceptorTranslation = {
      val rtip = getDbls(TagFromName.XRayImageReceptorTranslation)
      val comment = "%% XRayImageReceptorTranslation values"
      val x = name + "TX = " + rtip(0) + ";"
      val y = name + "TY = " + rtip(1) + ";"
      comment + ls + x + ls + y
    }

    val gantryAngle = getDbls(TagFromName.GantryAngle).head
    val gantryAngleText = "gantryAngle" + name + " = " + gantryAngle + ";"

    val xy = {
      if (isVert(result.al))
        "epidIsoX = cos(deg2rad(gantryAngleVert)) * (epidIsoVertX - VertTX);"
      else
        "epidIsoY = sin(deg2rad(gantryAngleHorz)) * (epidIsoHorzX - HorzTX);"
    }

    val z = "epid" + name + "Z = (-epidIso" + name + "Y) - " + name + "TY;"

    val summary = {
      val ga = Util.angleRoundedTo90(Util.gantryAngle(result.al))
      if (isVert(result.al)) {
        """fprintf("MV G %d (BB - DIGITAL_CAX) @ ISOCENTER PLANE:  %f   NA  %f  %f\n", """ + ga + ", epidIsoX, epidVertZ, sqrt(epidIsoX*epidIsoX + epidVertZ*epidVertZ));" + ls +
          """fprintf("MV G %d (BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f   NA  %f  %f\n", """ + ga + ", epidIsoX - cbctX, epidVertZ - cbctZ, sqrt((epidIsoX - cbctX)*(epidIsoX - cbctX) + (epidVertZ - cbctZ)*(epidVertZ - cbctZ)));"
      } else {
        """fprintf("MV G %d (BB - DIGITAL_CAX) @ ISOCENTER PLANE:  NA   %f  %f  %f\n", """ + ga + ", epidIsoY, epidHorzZ, sqrt(epidIsoY*epidIsoY + epidHorzZ*epidHorzZ));" + ls +
          """fprintf("MV G %d (BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f   NA  %f  %f\n", """ + ga + ", epidIsoY - cbctY, epidHorzZ - cbctZ, sqrt((epidIsoY - cbctY)*(epidIsoY - cbctY) + (epidHorzZ - cbctZ)*(epidHorzZ - cbctZ)));"
      }
    }

    val text = Seq(epidComment, RTImageSID, RadiationMachineSAD,
      ImagePlanePixelSpacing, RTImagePosition, pix, divergence,
      topLeft, iso, XRayImageReceptorTranslation,
      gantryAngleText, xy, z, summary).mkString(ls + ls)

    text
  }

  private def constructCompositeMatlab(epidResultList: Seq[(BBbyEPID, BBbyEPIDImageAnalysis.Result)], bbByCBCT: BBbyCBCT): String = {
    val separator = ls + ls + "%% ------------------------------------------------------------------" + ls + ls

    val epidText = epidResultList.map(er => constructEpidMatlab(er._2))

    val z = "epidIsoZ = (epidVertZ + epidHorzZ) / 2.0;"

    val cbctXYZ = {
      "%% Results from CBCT" + ls +
        "cbctX = " + bbByCBCT.cbctX_mm + " - " + bbByCBCT.rtplanX_mm + ";" + ls +
        "cbctY = " + bbByCBCT.cbctY_mm + " - " + bbByCBCT.rtplanY_mm + ";" + ls +
        "cbctZ = " + bbByCBCT.cbctZ_mm + " - " + bbByCBCT.rtplanZ_mm + ";" + ls +
        """fprintf("CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f  %f  %f\n", cbctX, cbctY, cbctZ);"""
    }

    val answer = {
      "epidMinuscbctX = epidIsoX - cbctX;" + ls +
        "epidMinuscbctY = epidIsoY - cbctY;" + ls +
        "epidMinuscbctZ = epidIsoZ - cbctZ;" + ls + ls +
        """fprintf("AVERAGE MV(BB - DIGITAL_CAX) @ ISOCENTER PLANE - CBCT(BB - DIGITAL_PLANNED_ISOCENTER):  %f   %f  %f  %f\n", """ +
        "epidIsoX - cbctX, epidIsoY - cbctY, epidIsoZ - cbctZ, sqrt((epidIsoX - cbctX)*(epidIsoX - cbctX) + (epidIsoY - cbctY)*(epidIsoY - cbctY) + (epidIsoZ - cbctZ)*(epidIsoZ - cbctZ)));"
    }

    val textList = Seq(cbctXYZ) ++ epidText ++ Seq(z, answer)

    val allText = textList.mkString(separator) + ls + ls
    allText
  }

  private def constructComposite(epidResultList: Seq[(BBbyEPID, BBbyEPIDImageAnalysis.Result)], extendedData: ExtendedData, runReq: BBbyEPIDRunReq): Either[String, (BBbyEPIDComposite, Option[String])] = {

    val bbByEPIDList = epidResultList.map(er => er._1)

    def getByAngleType(angleType: AngleType.Value) = {
      val at = angleType.toString
      def sameType(bbByEPID: BBbyEPID): Boolean = {
        val angTyp = AngleType.classifyAngle(bbByEPID.gantryAngle_deg)
        angTyp.isDefined && angTyp.get.toString.equals(at)
      }

      bbByEPIDList.filter(bbByEPID => sameType(bbByEPID))
    }

    val vert = getByAngleType(AngleType.vertical)
    val horz = getByAngleType(AngleType.horizontal)

    if ((vert.nonEmpty && horz.nonEmpty)) {
      // Use the same number of vertical and horizontal beams to get the averages.  Handles cases where are there are many of one and few of the others.
      val max = Math.min(vert.size, horz.size) // maximum number of images to use in each of the vertical and horizontal directions.
      val x_mm = vert.take(max).map(bb => bb.epid3DX_mm).sum / max
      val y_mm = horz.take(max).map(bb => bb.epid3DY_mm).sum / max
      val z_mm = bbByEPIDList.map(bb => bb.epid3DZ_mm).sum / bbByEPIDList.size
      val offset_mm = (new Point3d(x_mm, y_mm, z_mm)).distance(new Point3d(0, 0, 0))

      val SeriesInstanceUID = runReq.epidList.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

      val bbByCBCTHistory: Option[BBbyCBCT.BBbyCBCTHistory] = {
        BBbyCBCT.getProcedurePK match {
          case Some(cbctProcPk) => {
            val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
            val epidDateTime = extendedData.output.dataDate.get
            val epidDateFormatted = dateFormat.format(epidDateTime)
            val list = BBbyCBCT.history(extendedData.machine.machinePK.get, cbctProcPk)
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

      val matlabComposite = if (bbByCBCTHistory.isDefined) Some(constructCompositeMatlab(epidResultList, bbByCBCTHistory.get.bbByCBCT)) else None

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
            tableXlateral_mm = Some(bbByEPIDList.head.tableXlateral_mm - c.tableXlateral_mm),
            tableYvertical_mm = Some(bbByEPIDList.head.tableYvertical_mm - c.tableYvertical_mm),
            tableZlongitudinal_mm = Some(bbByEPIDList.head.tableZlongitudinal_mm - c.tableZlongitudinal_mm))
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

  def runProcedure(extendedData: ExtendedData, runReq: BBbyEPIDRunReq): ProcedureStatus.Value = {

    try {
      logger.info("Starting analysis of EPID Alignment for machine " + extendedData.machine.id)

      val bbLocList = runReq.epidList.par.map(epid => BBbyEPIDImageAnalysis.findBB(epid)).toList

      val resultList = bbLocList.filter(er => er.isRight).map(er => er.right.get)
      val epidResultList = resultList.map(result => (toBBbyEPID(result.al, result.iso, extendedData), result))
      val epidList = epidResultList.map(er => er._1)

      logger.info("Inserting EPID records into database")
      BBbyEPID.insertSeq(epidResultList.map(er => er._1))

      logger.info("Calculating composite result.")
      val bbByEPIDComposite = constructComposite(epidResultList, extendedData, runReq) // TODO this can fail if BB not found
      if (bbByEPIDComposite.isRight) {
        logger.info("Inserting composite EPID record into database: " + bbByEPIDComposite.right.get)
        bbByEPIDComposite.right.get._1.insert
      } else
        logger.info("No composite EPID record created.  Reported error: " + bbByEPIDComposite.left.get)

      val procedureStatus = if (bbByEPIDComposite.isRight) ProcedureStatus.done else ProcedureStatus.fail

      BBbyEPIDHTML.generateHtml(extendedData, epidList, bbByEPIDComposite, runReq, ProcedureStatus.done) // TODO status should be real
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