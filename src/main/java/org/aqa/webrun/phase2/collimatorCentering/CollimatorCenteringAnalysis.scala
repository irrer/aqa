package org.aqa.webrun.phase2.collimatorCentering

import org.aqa.Logging
import org.aqa.db.CollimatorCentering
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateEdge
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.IsoImagePlaneTranslator
import org.aqa.IsoImagePlaneTranslator
import org.aqa.IsoImagePlaneTranslator
import java.awt.Point

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorCenteringAnalysis extends Logging {

  val subProcedureName = "Collimator Centering"

  case class CollimatorCenteringResult(sum: Elem, sts: ProcedureStatus.Value, result: CollimatorCentering) extends SubProcedureResult(sum, sts, subProcedureName)

  /**
   * Perform actual analysis.
   */
  private def analyze(dicomFile090: DicomFile, dicomFile270: DicomFile, image090: DicomImage, image270: DicomImage, outputPK: Long): (CollimatorCentering, MeasureTBLREdges.AnalysisResult, MeasureTBLREdges.AnalysisResult) = {
    val al090 = dicomFile090.attributeList.get
    val al270 = dicomFile270.attributeList.get
    val translator = new IsoImagePlaneTranslator(al090)

    // Calculate edges in parallel for efficiency.
    val resultPair = {
      val pointZero = new Point(0, 0)
      def m090 = MeasureTBLREdges.measure(image090, translator, Util.collimatorAngle(al090), image090, pointZero)
      def m270 = MeasureTBLREdges.measure(image270, translator, Util.collimatorAngle(al270), image270, pointZero)
      //val rp = ParSeq(m090 _, m270 _).map(f => f()).toList  TODO put back in
      val rp = Seq(m090 _, m270 _).map(f => f()).toList
      rp
    }
    val result090 = resultPair(0)
    val result270 = resultPair(1)

    Trace.trace("result090: " + result090) // TODO rm

    val m090 = result090.measurementSet // in pixels
    val m270 = result270.measurementSet // in pixels
    val xCntrPix = (m090.center.getX + m270.center.getX) / 2 // in pixels
    val yCntrPix = (m090.center.getY + m270.center.getY) / 2 // in pixels
    val measuredCntrIso = translator.pix2Iso(xCntrPix, yCntrPix) // place in isoplane that is measured by collimator centering
    val plannedCntrIso = new Point2D.Double(0, 0)

    val errDistance = plannedCntrIso.distance(measuredCntrIso)
    val pass: Boolean = errDistance <= Config.CollimatorCenteringTolerence_mm
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    logger.info("CollimatorCentering error in mm: " + errDistance + "    Status: " + procedureStatus)

    val xy090 = m090.toX1X2Y1Y2(Util.collimatorAngle(al090)).pix2iso(translator)
    val xy270 = m270.toX1X2Y1Y2(Util.collimatorAngle(al270)).pix2iso(translator)

    val collimatorCentering = new CollimatorCentering(None, outputPK, procedureStatus.name,
      Util.sopOfAl(al090), Util.sopOfAl(al270), // SOPInstanceUID090, SOPInstanceUID270
      measuredCntrIso.getX - plannedCntrIso.getX, measuredCntrIso.getY - plannedCntrIso.getY, // xCollimatorCenterMinusImageCenter_mm, yCollimatorCenterMinusImageCenter_mm
      measuredCntrIso.getX, measuredCntrIso.getY, // xCollimatorCenter_mm, yCollimatorCenter_mm
      xy090.X1, xy090.X2, xy090.Y1, xy090.Y2,
      xy270.X1, xy270.X2, xy270.Y1, xy270.Y2)

    (collimatorCentering, result090, result270)
  }

  /**
   * For testing only
   */
  def testAnalyze(dicomFile090: DicomFile, dicomFile270: DicomFile, image090: DicomImage, image270: DicomImage, outputPK: Long): (CollimatorCentering, MeasureTBLREdges.AnalysisResult, MeasureTBLREdges.AnalysisResult) = {
    analyze(dicomFile090, dicomFile270, image090, image270, outputPK)
  }

  /**
   * Run the CollimatorCentering sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, CollimatorCenteringResult] = {
    try {
      logger.info("Starting analysis of CollimatorCentering")
      val al090 = runReq.rtimageMap(Config.CollimatorCentering090BeamName).attributeList.get
      val al270 = runReq.rtimageMap(Config.CollimatorCentering270BeamName).attributeList.get
      val translator = new IsoImagePlaneTranslator(al090)

      val image090 = runReq.derivedMap(Config.CollimatorCentering090BeamName)
      val image270 = runReq.derivedMap(Config.CollimatorCentering270BeamName)

      val analysisResult = analyze(runReq.rtimageMap(Config.CollimatorCentering090BeamName), runReq.rtimageMap(Config.CollimatorCentering270BeamName),
        image090.pixelCorrectedImage, image270.pixelCorrectedImage,
        extendedData.output.outputPK.get)

      val collimatorCentering = analysisResult._1
      val result090 = analysisResult._2
      val result270 = analysisResult._3

      val procedureStatus = if (collimatorCentering.status.equals(ProcedureStatus.pass.toString)) ProcedureStatus.pass else ProcedureStatus.fail

      logger.info("Inserting CollimatorCentering row: " + collimatorCentering)
      collimatorCentering.insert

      val elem = CollimatorCenteringHTML.makeDisplay(extendedData, collimatorCentering, procedureStatus, result090, result270, runReq)
      logger.info("Finished processing for " + subProcedureName)
      val result = Right(new CollimatorCenteringResult(elem, procedureStatus, collimatorCentering))
      logger.info("Finished analysis of CollimatorCentering")
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorCentering: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
