package org.aqa.webrun.phase2.collimatorPosition

import org.aqa.Logging
import org.aqa.db.CollimatorPosition
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
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.IsoImagePlaneTranslator

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorPositionAnalysis extends Logging {

  /**
   * Measure the four collimator edges.  Function is public to make it testable.
   */
  private def measureImage(beamName: String, FloodCompensation: Boolean, biasAndPixelCorrectedCroppedImage: DicomImage, pixelCorrectedImage: DicomImage,
    al: AttributeList, originalImage: DicomImage, outputPK: Long, floodOffset: Point): Either[String, (CollimatorPosition, BufferedImage)] = {
    try {
      val collimatorAngle = Util.collimatorAngle(al)
      val gantryAngle = Util.gantryAngle(al)

      def dbl(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head

      val translator = new IsoImagePlaneTranslator(al)
      val edges = {
        if (FloodCompensation)
          MeasureTBLREdges.measure(biasAndPixelCorrectedCroppedImage, translator, collimatorAngle, originalImage, floodOffset, Config.PenumbraThresholdPercent / 100)
        else
          MeasureTBLREdges.measure(pixelCorrectedImage, translator, collimatorAngle, originalImage, new Point(0, 0), Config.PenumbraThresholdPercent / 100)
      }

      val expectedEdgesX1X2Y1Y2 = MeasureTBLREdges.imageCollimatorPositions(al)
      val expectedEdgesTBLR = expectedEdgesX1X2Y1Y2.toTBLR(collimatorAngle)
      val floodOff = if (FloodCompensation) floodOffset else new Point(0, 0)
      val measuredTBLR = edges.measurementSet.floodRelative(floodOff).pix2iso(translator)
      val measuredX1X2Y1Y2 = measuredTBLR.toX1X2Y1Y2(collimatorAngle)

      val expMinusMeasured = expectedEdgesTBLR.minus(measuredTBLR).toX1X2Y1Y2(collimatorAngle)
      logger.info("Beam " + beamName + " flood Comp: " + FloodCompensation +
        "\n    expected edges: " + expectedEdgesX1X2Y1Y2 +
        "\n    measured edges: " + measuredX1X2Y1Y2 +
        "\n    expected - measured: " + expMinusMeasured)

      val worst = expMinusMeasured.toSeq.map(m => m.abs).max

      val status = if (worst > Config.CollimatorCenteringTolerence_mm) ProcedureStatus.fail else ProcedureStatus.pass

      val uid = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString

      val colPosn = new CollimatorPosition(None, outputPK, status.toString, uid, beamName, FloodCompensation,
        measuredX1X2Y1Y2.X1,
        measuredX1X2Y1Y2.X2,
        measuredX1X2Y1Y2.Y1,
        measuredX1X2Y1Y2.Y2,
        expMinusMeasured.X1,
        expMinusMeasured.X2,
        expMinusMeasured.Y1,
        expMinusMeasured.Y2,
        gantryAngle, collimatorAngle)

      logger.info("CollimatorPosition\n" + colPosn)

      Right(colPosn, edges.bufferedImage)
    } catch {
      case t: Throwable => {
        val msg = "Unexpected error while analyzing " + beamName + " for collimator position."
        logger.warn(msg + " : " + t + "\n" + fmtEx(t))
        Left(msg)
      }
    }

  }

  /**
   *  For testing only.
   */
  def testMeasureImage(beamName: String, FloodCompensation: Boolean, biasAndPixelCorrectedCroppedImage: DicomImage, pixelCorrectedImage: DicomImage,
    al: AttributeList, originalImage: DicomImage, outputPK: Long, floodOffset: Point): Either[String, (CollimatorPosition, BufferedImage)] = {
    measureImage(beamName, FloodCompensation, biasAndPixelCorrectedCroppedImage, pixelCorrectedImage, al, originalImage, outputPK, floodOffset)
  }

  val subProcedureName = "Collimator Position"

  case class CollimatorPositionResult(sum: Elem, sts: ProcedureStatus.Value, resultList: Seq[CollimatorPosition], crashList: Seq[String]) extends SubProcedureResult(sum, sts, subProcedureName)

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, CollimatorPositionResult] = {
    try {
      logger.info("Starting analysis of CollimatorPosition")
      val posnBeams = Config.CollimatorPositionBeamList.filter(cp => runReq.derivedMap.contains(cp.beamName))
      val resultList = posnBeams.par.map(cp => measureImage(
        cp.beamName,
        cp.FloodCompensation,
        runReq.derivedMap(cp.beamName).biasAndPixelCorrectedCroppedImage,
        runReq.derivedMap(cp.beamName).pixelCorrectedImage,
        runReq.derivedMap(cp.beamName).attributeList,
        runReq.derivedMap(cp.beamName).originalImage,
        extendedData.output.outputPK.get,
        runReq.floodOffset)).toList

      val doneList = resultList.filter(r => r.isRight).map(r => r.right.get)
      val crashList = resultList.filter(l => l.isLeft).map(l => l.left.get)

      // To pass (succeed), there must be no crashes and all successfully processed beams must pass.
      val pass = crashList.isEmpty && doneList.map(d => d._1.status.toString.equals(ProcedureStatus.pass.toString)).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      val doneDataList = doneList.map(r => r._1)
      CollimatorPosition.insert(doneDataList) // save to database
      logger.info("Inserted  " + doneList.size + " + CollimatorPosition rows.")

      // TODO Should make nice HTML for each buffered images.
      val elem = CollimatorPositionHTML.makeDisplay(extendedData, runReq, doneList, crashList, procedureStatus)
      val result = Right(new CollimatorPositionResult(elem, procedureStatus, doneDataList, crashList))
      logger.info("Finished analysis of CollimatorPosition.  Status: " + procedureStatus)
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
