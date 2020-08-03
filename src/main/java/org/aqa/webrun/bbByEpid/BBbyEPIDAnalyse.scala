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
  private def toBBbyEPID(epid: AttributeList, bbLocation: Either[String, Point2d], extendedData: ExtendedData): Option[BBbyEPID] = {
    if (bbLocation.isRight) {
      val bbLoc = bbLocation.right.get
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

      val epid3DX_mm = Math.cos(gantryAngle_rad) * (bbLoc.getX - epidOffset.getX)
      val epid3DY_mm = Math.sin(gantryAngle_rad) * (bbLoc.getX - epidOffset.getX)
      val epid3DZ_mm = (-bbLoc.getY) - epidOffset.getY // flip sign to directionally match coordinate system
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
        epidImageX_mm = bbLoc.getX,
        epidImageY_mm = bbLoc.getY,
        epid3DX_mm, epid3DY_mm, epid3DZ_mm,
        getDbl(TagFromName.TableTopLateralPosition), // tableXlateral_mm
        getDbl(TagFromName.TableTopVerticalPosition), // tableYvertical_mm
        getDbl(TagFromName.TableTopLongitudinalPosition)) // tableZlongitudinal_mm

      Some(bbByEPID)
    } else
      None
  }

  private def constructComposite(bbByEPIDList: Seq[BBbyEPID], extendedData: ExtendedData, runReq: BBbyEPIDRunReq): Either[String, BBbyEPIDComposite] = {

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
          // BBbyCBCT.getProcedurePK not defined,  Must be that there are no BBbyCBCT rows.
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

      Right(bbByEPIDCompositeFinal)
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

      //val bbLocList = runReq.epidList.par.map(epid => BBbyEPIDImageAnalysis.findBB(epid)).toList  // TODO put back
      val bbLocList = runReq.epidList.map(epid => BBbyEPIDImageAnalysis.findBB(epid)).toList // TODO rm

      val dbList = runReq.epidList.zip(bbLocList).map(er => toBBbyEPID(er._1, er._2, extendedData))

      logger.info("Inserting EPID records into database")
      BBbyEPID.insertSeq(dbList.flatten)

      logger.info("Calculating composite result.")
      val bbByEPIDComposite = constructComposite(dbList.flatten, extendedData, runReq) // TODO this can fail if BB not found
      if (bbByEPIDComposite.isRight) {
        logger.info("Inserting composite EPID record into database: " + bbByEPIDComposite.right.get)
        bbByEPIDComposite.right.get.insert
      } else
        logger.info("No composite EPID record created.  Reported error: " + bbByEPIDComposite.left.get)

      val procedureStatus = if (bbByEPIDComposite.isRight) ProcedureStatus.done else ProcedureStatus.fail

      BBbyEPIDHTML.generateHtml(extendedData, dbList, bbByEPIDComposite, runReq, ProcedureStatus.done) // TODO status should be real
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