package org.aqa.webrun.bbByEpid

import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import javax.vecmath.Point2d
import org.aqa.db.BBbyEPID

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

  private def toBBbyEPID(epid: AttributeList, bbLocation: Either[String, Point2d]): Option[BBbyEPID] = {
    if (bbLocation.isRight) {
      val angle = Math.toRadians(Util.gantryAngle(epid))

    } else None
    ???
  }

  def runProcedure(extendedData: ExtendedData, runReq: BBbyEPIDRunReq): Unit = {

    def getAngleIndex(angleType: BBbyEPIDRun.AngleType.Value) = {
      runReq.epidList.indexWhere(epid => BBbyEPIDRun.isAngleType(epid, angleType))
    }

    try {
      logger.info("Starting analysis of EPID Alignment")

      val resultList = runReq.epidList.par.map(epid => BBbyEPIDImageAnalysis.findBB(epid)).toList

      val dbList = runReq.epidList.zip(resultList).map(er => toBBbyEPID(er._1, er._2))

      val horzIndex = getAngleIndex(BBbyEPIDRun.AngleType.horizontal)
      val vertIndex = getAngleIndex(BBbyEPIDRun.AngleType.vertical)

      logger.info("Finished analysis of EPID Alignment")
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of BBbyEPIDAnalyse" + ": " + t + fmtEx(t))
      }
    }
  }

}