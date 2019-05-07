package org.aqa.webrun.phase2.centerDose

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
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.db.CollimatorCentering

object CenterDoseAnalysis extends Logging {

  /**
   * Construct a CenterDose
   */
  private def constructCenterDose(beamName: String, pointList: Seq[Point], outputPK: Long, dicomImage: DicomImage, attributeList: AttributeList): CenterDose = {
    val dose = Phase2Util.measureDose(pointList, dicomImage, attributeList)
    val SOPInstanceUID = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
    val units = attributeList.get(TagFromName.RescaleType).getSingleStringValueOrEmptyString
    new CenterDose(None, outputPK, SOPInstanceUID, beamName, dose, units)
  }

  private def analyse(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Seq[CenterDose] = {
    val pointList = Phase2Util.makeCenterDosePointList(runReq.flood.attributeList.get, collimatorCentering.center)
    val outputPK = extendedData.output.outputPK.get

    val availableBeamList = Config.CenterDoseBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))
    val resultList = availableBeamList.map(beamName => constructCenterDose(beamName, pointList, outputPK, runReq.derivedMap(beamName).originalImage, runReq.rtimageMap(beamName).attributeList.get))
    logger.info("Number of CenterDose results for " + resultList.size)
    logger.info("CenterDose results: " + resultList.mkString("\n"))
    resultList
  }

  /**
   * For testing only.
   */
  def testConstructCenterDose(beamName: String, dicomFile: DicomFile): CenterDose = {
    val attributeList = dicomFile.attributeList.get
    val pointList = Phase2Util.makeCenterDosePointList(attributeList, new Point2D.Double(0, 0))
    val outputPK = -1
    constructCenterDose(beamName, pointList, outputPK, new DicomImage(attributeList), attributeList)
  }

  private val subProcedureName = "Center Dose"

  case class CenterDoseResult(summry: Elem, stats: ProcedureStatus.Value, resultList: Seq[CenterDose]) extends SubProcedureResult(summry, stats, subProcedureName)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, CenterDoseResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CenterDose")
      val status = ProcedureStatus.done
      val resultList = analyse(extendedData, runReq, collimatorCentering)
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
