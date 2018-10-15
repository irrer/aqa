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

  private def makePointList(attributeList: AttributeList): Seq[Point] = {
    val spacing = Phase2Util.getImagePlanePixelSpacing(attributeList)
    val width = attributeList.get(TagFromName.Columns).getIntegerValues().head
    val height = attributeList.get(TagFromName.Rows).getIntegerValues().head

    // get center of image, accounting for 1/2 pixel offset
    val xCenter = (width / 2.0) + 0.5 // in pixels
    val yCenter = (height / 2.0) + 0.5 // in pixels

    // center of image in mm
    val center = new Point2D.Double(xCenter * spacing.getX, yCenter * spacing.getY)

    val xRadius = (Config.CenterDoseRadius_mm / spacing.getX).toInt + 2 // in pixels
    val yRadius = (Config.CenterDoseRadius_mm / spacing.getY).toInt + 2 // in pixels

    val xRange = (xCenter - xRadius).floor.toInt to (xCenter + xRadius).ceil.toInt // pixel range
    val yRange = (yCenter - yRadius).floor.toInt to (yCenter + yRadius).ceil.toInt // pixel range

    // step through pixels and see which are close enough.  Both x and y are in pixels.
    def nearCenter(x: Int, y: Int): Boolean = center.distance(x * spacing.getX, y * spacing.getY) <= Config.CenterDoseRadius_mm

    val pointList = for (x <- xRange; y <- yRange; if nearCenter(x, y)) yield { new Point(x, y) }
    pointList
  }

  private def analyse(extendedData: ExtendedData, runReq: RunReq): Seq[CenterDose] = {
    val pointList = makePointList(runReq.flood.attributeList.get)
    val outputPK = extendedData.output.outputPK.get

    /**
     * Average the pixels at the given points.
     */
    def avg(dicomImage: DicomImage): Double = {
      pointList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pointList.size
    }

    /**
     * Construct a CenterDose
     */
    def measure(beamName: String, dicomImage: DicomImage, attributeList: AttributeList): CenterDose = {
      val m = attributeList.get(TagFromName.RescaleSlope).getDoubleValues().head
      val b = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues().head
      val dose = (avg(dicomImage) * m) + b
      val SOPInstanceUID = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
      val units = attributeList.get(TagFromName.RescaleType).getSingleStringValueOrEmptyString
      new CenterDose(None, outputPK, SOPInstanceUID, beamName, dose, units)
    }

    val centerDoseFlood = measure(Config.FloodFieldBeamName, runReq.floodOriginalImage, runReq.flood.attributeList.get)
    val availableBeamList = Config.CenterDoseBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))
    val centerDoseList = availableBeamList.map(beamName => measure(beamName, runReq.derivedMap(beamName).originalImage, runReq.rtimageMap(beamName).attributeList.get))
    centerDoseFlood +: centerDoseList
  }

  private val subProcedureName = "Center Dose"

  class CenterDoseResult(summary: Elem, status: ProcedureStatus.Value, resultList: Seq[CenterDose]) extends SubProcedureResult(summary, status, subProcedureName)

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