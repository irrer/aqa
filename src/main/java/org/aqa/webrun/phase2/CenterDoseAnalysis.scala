package org.aqa.webrun.phase2

import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.Institution
import org.aqa.db.Input
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.Util
import java.util.Date
import org.aqa.web.WebServer
import org.aqa.db.CenterDose
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus
import java.io.File
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeList
import java.awt.Point
import com.pixelmed.dicom.TagFromName
import org.aqa.Logging

object CenterDoseAnalysis extends Logging {

  /**
   * Construct a CenterDose
   */
  def constructCenterDose(beamName: String, pointList: Seq[Point], outputPK: Long, dicomImage: DicomImage, attributeList: AttributeList): CenterDose = {
    val dose = Phase2Util.measureDose(pointList, dicomImage, attributeList)
    val SOPInstanceUID = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
    val units = attributeList.get(TagFromName.RescaleType).getSingleStringValueOrEmptyString
    new CenterDose(None, outputPK, SOPInstanceUID, beamName, dose, units)
  }

  private def analyse(extendedData: ExtendedData, runReq: RunReq): Seq[CenterDose] = {
    val pointList = Phase2Util.makeCenterDosePointList(runReq.flood.attributeList.get)
    val outputPK = extendedData.output.outputPK.get

    val centerDoseFlood = constructCenterDose(Config.FloodFieldBeamName, pointList, outputPK, runReq.floodOriginalImage, runReq.flood.attributeList.get)
    val availableBeamList = Config.CenterDoseBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))
    val centerDoseList = availableBeamList.map(beamName => constructCenterDose(beamName, pointList, outputPK, runReq.derivedMap(beamName).originalImage, runReq.rtimageMap(beamName).attributeList.get))
    centerDoseFlood +: centerDoseList
  }

  private val subProcedureName = "Center Dose"

  case class CenterDoseResult(summry: Elem, stats: ProcedureStatus.Value, resultList: Seq[CenterDose]) extends SubProcedureResult(summry, stats, subProcedureName)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, CenterDoseResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CenterDose")
      val status = ProcedureStatus.done
      val resultList = analyse(extendedData, runReq)
      logger.info("Storing results for " + resultList.size + " CenterDose rows")
      logger.info("CenterDose results: " + resultList.mkString("\n"))
      CenterDose.insert(resultList)
      val summary = CenterDoseHTML.makeDisplay(extendedData, runReq, resultList, status)
      val result = Right(new CenterDoseResult(summary, status, resultList))
      logger.info("Finished analysis of CenterDose")
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CenterDose: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }

}