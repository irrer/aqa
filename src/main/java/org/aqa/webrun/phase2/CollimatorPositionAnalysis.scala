package org.aqa.webrun.phase2

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
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorPositionAnalysis extends Logging {

  /**
   * Measure the four collimator edges.
   */
  private def measureImage(beamName: String, FloodCompensation: Boolean, extendedData: ExtendedData, runReq: RunReq): Either[String, (CollimatorPosition, BufferedImage)] = {
    try {
      val derived = runReq.derivedMap(beamName)

      val al = derived.dicomFile.attributeList.get
      val collimatorAngle = Util.collimatorAngle(al)
      val gantryAngle = Util.gantryAngle(al)

      def dbl(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head

      val edges = {
        if (FloodCompensation)
          MeasureTBLREdges.measure(derived.biasAndPixelCorrectedCroppedImage, runReq.ImagePlanePixelSpacing, collimatorAngle, derived.originalImage, runReq.floodOffset, Config.PenumbraThresholdPercent / 100)
        else
          MeasureTBLREdges.measure(derived.pixelCorrectedImage, runReq.ImagePlanePixelSpacing, collimatorAngle, derived.originalImage, new Point(0, 0), Config.PenumbraThresholdPercent / 100)
      }

      val spacing = Phase2Util.getImagePlanePixelSpacing(al)

      val center = new Point2D.Double(
        ((derived.originalImage.width - 1) * spacing.getX) / 2,
        ((derived.originalImage.width - 1) * spacing.getY) / 2)

      val divergence = dbl(TagFromName.RTImageSID) / dbl(TagFromName.RadiationMachineSAD)

      val scaledEdges = new MeasureTBLREdges.TBLR(
        (edges.measurementSet.top - center.getY) / divergence,
        (edges.measurementSet.bottom - center.getY) / divergence,
        (edges.measurementSet.left - center.getX) / divergence,
        (edges.measurementSet.right - center.getX) / divergence)

      // TODO val orientedScaledEdges = MeasureTBLREdges.TBLRtoX1X2Y1Y2(collimatorAngle, scaledEdges)
      val orientedScaledEdges = MeasureTBLREdges.TBLRtoX1X2Y1Y2(270, scaledEdges) // TODO always use the 'do nothing' angle?

      val expectedEdges = MeasureTBLREdges.imageCollimatorPositions(al) //   planCollimatorPositions(beamName, runReq.rtplan.attributeList.get)
      logger.info("Beam " + beamName + "  expected edges: " + expectedEdges)

      val expMinusMeasured = expectedEdges.minus(orientedScaledEdges)

      val worst = expMinusMeasured.toSeq.map(m => m.abs).max

      val status = if (worst > Config.CollimatorCenteringTolerence_mm) ProcedureStatus.fail else ProcedureStatus.pass

      val uid = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString

      val colPosn = new CollimatorPosition(None, extendedData.output.outputPK.get, status.toString, uid, beamName, FloodCompensation,
        orientedScaledEdges.X1,
        orientedScaledEdges.X2,
        orientedScaledEdges.Y1,
        orientedScaledEdges.Y2,
        expMinusMeasured.X1,
        expMinusMeasured.X2,
        expMinusMeasured.Y1,
        expMinusMeasured.Y2,
        gantryAngle, collimatorAngle)

      Right(colPosn, edges.bufferedImage)
    } catch {
      case t: Throwable => {
        val msg = "Unexpected error while analyzing " + beamName + " for collimator position."
        logger.warn(msg + " : " + t + "\n" + fmtEx(t))
        Left(msg)
      }
    }

  }

  val subProcedureName = "Collimator Position"

  case class CollimatorPositionResult(sum: Elem, sts: ProcedureStatus.Value, resultList: Seq[CollimatorPosition], crashList: Seq[String]) extends SubProcedureResult(sum, sts, subProcedureName)

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, CollimatorPositionResult] = {
    try {
      logger.info("Starting analysis of CollimatorPosition")
      // TODO put back in
      //val resultList = Config.CollimatorPositionBeamList.filter(cp => runReq.derivedMap.contains(cp.beamName)).par.map(cp => measureImage(cp.beamName, cp.FloodCompensation, extendedData, runReq)).toList
      val resultList = Config.CollimatorPositionBeamList.filter(cp => runReq.derivedMap.contains(cp.beamName)).map(cp => measureImage(cp.beamName, cp.FloodCompensation, extendedData, runReq)).toList

      val doneList = resultList.filter(r => r.isRight).map(r => r.right.get)
      val crashList = resultList.filter(l => l.isLeft).map(l => l.left.get)

      // To pass (succeed), there must be no crashes and all successfully processed beams must pass.
      val pass = crashList.isEmpty && doneList.map(d => d._1.status.toString.equals(ProcedureStatus.pass.toString)).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      val doneDataList = doneList.map(r => r._1)
      CollimatorPosition.insert(doneDataList) // save to database
      logger.info("Inserted  " + doneList.size + " + CollimatorPosition rows.")

      // TODO currently buffered images are created but not used.  Not sure it is worth it to show them to the user or not.  Also they
      //     do not have the X1X2Y1Y2 labels, nor the difference between measured and expected.
      val elem = CollimatorPositionHTML.makeDisplay(extendedData, runReq, doneDataList, crashList, procedureStatus)
      val result = Right(new CollimatorPositionResult(elem, procedureStatus, doneDataList, crashList))
      logger.info("Starting analysis of CollimatorPosition")
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
