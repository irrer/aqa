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

/**
 * Given validated data, process it.
 */
object BBbyEPIDAnalyse extends Logging {

  /**
   * Get the SOP of the plan referenced by the given EPID.
   */
  private def getPlanRef(epid: AttributeList): String = {
    val seq = DicomUtil.seqToAttr(epid, TagFromName.ReferencedRTPlanSequence)
    seq.head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
  }

  private def toBBbyEPID(epid: AttributeList, bbLocation: Either[String, Point2d], extendedData: ExtendedData): Option[BBbyEPID] = {
    if (bbLocation.isRight) {
      val bbLoc = bbLocation.right.get
      val gantryAngle_deg = Util.gantryAngle(epid)
      val gantryAngle_rad = Math.toRadians(gantryAngle_deg)

      //      val RadiationMachineSAD = epid.get(TagFromName.RadiationMachineSAD).getDoubleValues.head
      //      val xGantry = sin * RadiationMachineSAD
      //      val yGantry = cos * RadiationMachineSAD

      val epid3DX_mm = Math.cos(gantryAngle_rad) * bbLoc.getX
      val epid3DY_mm = Math.sin(gantryAngle_rad) * bbLoc.getX
      val epid3DZ_mm = bbLoc.getY
      val origin = new Point3d(0, 0, 0)

      val bbByEPID = new BBbyEPID(
        bbByEPIDPK = None,
        outputPK = extendedData.output.outputPK.get,
        rtplanSOPInstanceUID = getPlanRef(epid),
        epidSOPInstanceUid = Util.sopOfAl(epid),
        offset_mm = (new Point3d(epid3DX_mm, epid3DY_mm, epid3DZ_mm)).distance(origin),
        gantryAngle_deg = gantryAngle_deg,
        status = ProcedureStatus.done.toString,
        epidImageX_mm = bbLoc.getX,
        epidImageY_mm = bbLoc.getY,
        epid3DX_mm,
        epid3DY_mm,
        epid3DZ_mm)
      Some(bbByEPID)
    } else
      None
  }

  private def constructComposite(bbByEPIDList: Seq[BBbyEPID], extendedData: ExtendedData, runReq: BBbyEPIDRunReq): BBbyEPIDComposite = {

    def getByAngleType(angleType: BBbyEPIDRun.AngleType.Value) = {
      val at = angleType.toString
      def sameType(bbByEPID: BBbyEPID): Boolean = {
        val angTyp = BBbyEPIDRun.classifyAngle(bbByEPID.gantryAngle_deg)
        angTyp.isDefined && angTyp.get.toString.equals(at)
      }

      bbByEPIDList.filter(bbByEPID => sameType(bbByEPID))
    }

    val vert = getByAngleType(BBbyEPIDRun.AngleType.vertical)
    val horz = getByAngleType(BBbyEPIDRun.AngleType.horizontal)

    val x_mm = vert.map(bb => bb.epid3DX_mm).sum / vert.size
    val y_mm = horz.map(bb => bb.epid3DY_mm).sum / horz.size
    val z_mm = bbByEPIDList.map(bb => bb.epid3DZ_mm).sum / bbByEPIDList.size
    val offset_mm = (new Point3d(x_mm, y_mm, z_mm)).distance(new Point3d(0, 0, 0))

    val SeriesInstanceUID = runReq.epidList.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

    val bbByEPIDComposite = new BBbyEPIDComposite(
      bbByEPIDCompositePK = None,
      outputPK = extendedData.output.outputPK.get,
      rtplanSOPInstanceUID = getPlanRef(runReq.epidList.head),
      epidSeriesInstanceUID = SeriesInstanceUID,
      offset_mm,
      x_mm,
      y_mm,
      z_mm)

    bbByEPIDComposite
  }

  def runProcedure(extendedData: ExtendedData, runReq: BBbyEPIDRunReq): ProcedureStatus.Value = {

    try {
      logger.info("Starting analysis of EPID Alignment")

      val bbLocList = runReq.epidList.par.map(epid => BBbyEPIDImageAnalysis.findBB(epid)).toList

      val dbList = runReq.epidList.zip(bbLocList).map(er => toBBbyEPID(er._1, er._2, extendedData))

      logger.info("Inserting EPID records into database")
      BBbyEPID.insertSeq(dbList.flatten)

      logger.info("Calculating composite result.")
      val bbByEPIDComposite = constructComposite(dbList.flatten, extendedData, runReq) // TODO this can fail if BB not found
      logger.info("Inserting composite EPID record into database: " + bbByEPIDComposite)
      bbByEPIDComposite.insert

      BBbyEPIDHTML.generateHtml(extendedData, dbList, Some(bbByEPIDComposite), runReq, ProcedureStatus.done) // TODO status should be real
      logger.info("Finished analysis of EPID Alignment")
      ProcedureStatus.done
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of BBbyEPIDAnalyse" + ": " + t + fmtEx(t))
        ProcedureStatus.crash
      }
    }
  }

}