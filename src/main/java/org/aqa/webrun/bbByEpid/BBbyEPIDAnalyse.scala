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

  def runProcedure(extendedData: ExtendedData, runReq: BBbyEPIDRunReq): ProcedureStatus.Value = {

    def getAngleIndex(angleType: BBbyEPIDRun.AngleType.Value) = {
      runReq.epidList.indexWhere(epid => BBbyEPIDRun.isAngleType(epid, angleType))
    }

    try {
      logger.info("Starting analysis of EPID Alignment")

      val bbLocList = runReq.epidList.par.map(epid => BBbyEPIDImageAnalysis.findBB(epid)).toList

      val dbList = runReq.epidList.zip(bbLocList).map(er => toBBbyEPID(er._1, er._2, extendedData))

      logger.info("Inserting EPID records into database")
      BBbyEPID.insertSeq(dbList.flatten)

      val horzIndex = getAngleIndex(BBbyEPIDRun.AngleType.horizontal)
      val vertIndex = getAngleIndex(BBbyEPIDRun.AngleType.vertical)

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