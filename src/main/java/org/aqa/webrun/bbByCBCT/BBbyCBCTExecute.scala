package org.aqa.webrun.bbByCBCT

import scala.xml.Elem
import javax.vecmath.Point3d
import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import com.pixelmed.dicom.AttributeList
import org.aqa.db.CollimatorCentering
import org.aqa.webrun.phase2.Phase2Util
import com.pixelmed.dicom.TagFromName
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import org.aqa.Util
import edu.umro.ImageUtil.LocateMax
import edu.umro.ImageUtil.DicomVolume
import javax.vecmath.Point3i
import edu.umro.ScalaUtil.Trace
import java.io.File
import edu.umro.ImageUtil.ImageUtil
import java.awt.image.BufferedImage
import java.awt.geom.Point2D
import org.aqa.VolumeTranslator
import java.awt.Color
import org.aqa.db.BBbyCBCT

object BBbyCBCTExecute extends Logging {

  private val subProcedureName = "CBCT Alignment"

  private def showFailure(message: String, extendedData: ExtendedData) = {
    // TODO
    ???
  }

  /**
   * Construct the database row and insert it.
   */
  private def saveToDb(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, bbPointInRtplan: Point3d): BBbyCBCT = {

    val rtplanIsocenter = Util.getPlanIsocenterList(runReq.rtplan).head

    val bbByCBCT = new BBbyCBCT(
      None, // bbByCBCTPK
      extendedData.output.outputPK.get, // outputPK
      Util.sopOfAl(runReq.rtplan), // rtplanSOPInstanceUID
      runReq.cbct.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString, // cbctSeriesInstanceUid
      rtplanIsocenter.distance(bbPointInRtplan), // offset_mm
      ProcedureStatus.done.toString, // status
      rtplanIsocenter.getX, // planX_mm
      rtplanIsocenter.getY, // planY_mm
      rtplanIsocenter.getZ, // planZ_mm
      bbPointInRtplan.getX, // cbctX_mm
      bbPointInRtplan.getY, // cbctY_mm
      bbPointInRtplan.getZ // cbctZ_mm
    )

    logger.info("Inserting BBbyCBCT into database: " + bbByCBCT)

    bbByCBCT.insert
    bbByCBCT
  }

  def runProcedure(extendedData: ExtendedData, runReq: BBbyCBCTRunReq): ProcedureStatus.Value = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CBCT Alignment")
      val result = BBbyCBCTAnalysis.volumeAnalysis(runReq.cbct)
      if (result.isRight) {
        val volumePoint = result.right.get._1
        val imageXYZ = result.right.get._2
        logger.info("Found BB in CBCT volume.  XYZ Coordinates in original CBCT space: " +
          Util.fmtDbl(volumePoint.getX) + ", " + Util.fmtDbl(volumePoint.getY) + ", " + Util.fmtDbl(volumePoint.getZ))

        val bbPointInRtplan = runReq.reg.transform(volumePoint)
        val rtplanIsocenter = Util.getPlanIsocenterList(runReq.rtplan).head
        val bbByCBCT = saveToDb(extendedData, runReq, bbPointInRtplan)
        //        val html = BBbyCBCTHTML.generateHtml(extendedData, bbByCBCT, imageXYZ)
        //        html
        logger.info("Finished analysis of CBCT Alignment")
        ProcedureStatus.done
      } else {
        showFailure(result.left.get, extendedData)
        ProcedureStatus.fail
      }
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of " + subProcedureName + ": " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
        ProcedureStatus.crash
      }
    }
  }
}