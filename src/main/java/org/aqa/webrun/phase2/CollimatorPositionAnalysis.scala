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
import org.aqa.webrun.phase2.MeasureNSEWEdges.NSEW
import org.aqa.db.CollimatorCentering

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object CollimatorPositionAnalysis extends Logging {

  def specifiesX(devType: String): Boolean = Seq("X", "ASYMX", "MLCX").contains(devType.toUpperCase)

  def specifiesY(devType: String): Boolean = Seq("Y", "ASYMY", "MLCY").contains(devType.toUpperCase)

  private def planCollimatorPositions(beamName: String, plan: AttributeList): NSEW = {
    val beamSeq = Util.seq2Attr(plan, TagFromName.BeamSequence).find(bs => bs.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.equals(beamName)).get
    val controlPtSeq = Util.seq2Attr(beamSeq, TagFromName.ControlPointSequence).head
    val devPosSeq = Util.seq2Attr(controlPtSeq, TagFromName.BeamLimitingDevicePositionSequence)

    def getPair(nameList: Seq[String]): Array[Double] = {
      devPosSeq.filter(s => nameList.contains(s.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString)).head.get(TagFromName.LeafJawPositions).getDoubleValues
    }

    val xPair = getPair(Util.xOrientation)
    val yPair = getPair(Util.yOrientation)

    new NSEW(xPair.min, xPair.max, yPair.min, yPair.max)
  }

  /**
   * Measure the four collimator edges.
   */
  private def measureImage(beamName: String, extendedData: ExtendedData, runReq: RunReq): Either[String, (CollimatorPosition, BufferedImage)] = {
    try {
      val derived = runReq.derivedMap(beamName)

      val al = derived.dicomFile.attributeList.get

      def dbl(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head

      val edges = MeasureNSEWEdges.measure(derived.biasAndPixelCorrectedCroppedImage, runReq.ImagePlanePixelSpacing, derived.originalImage, runReq.floodOffset, Config.PenumbraThresholdPercent / 100)
      val spacing = Phase2Util.getImagePlanePixelSpacing(al)

      val center = new Point2D.Double(
        ((derived.originalImage.width - 1) * spacing.getX) / 2,
        ((derived.originalImage.width - 1) * spacing.getY) / 2)

      val divergence = dbl(TagFromName.RTImageSID) / dbl(TagFromName.RadiationMachineSAD)

      val scaledEdges = new NSEW(
        (edges.measurementSet.north - center.getY) / divergence,
        (edges.measurementSet.south - center.getY) / divergence,
        (edges.measurementSet.east - center.getX) / divergence,
        (edges.measurementSet.west - center.getX) / divergence)

      val planEdges = planCollimatorPositions(beamName, runReq.rtplan.attributeList.get)
      logger.info("Beam " + beamName + "  plan edges: " + planEdges)

      val planMinusScaled = new NSEW(
        planEdges.north - scaledEdges.north,
        planEdges.south - scaledEdges.south,
        planEdges.east - scaledEdges.east,
        planEdges.west - scaledEdges.west)

      val worst = Seq(planMinusScaled.north, planMinusScaled.south, planMinusScaled.east, planMinusScaled.west).map(m => m.abs).max

      val status = if (worst > Config.CollimatorCenteringTolerence_mm) ProcedureStatus.fail else ProcedureStatus.pass

      val uid = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
      val gantryAngle = al.get(TagFromName.GantryAngle).getDoubleValues.head
      val collimatorAngle = al.get(TagFromName.BeamLimitingDeviceAngle).getDoubleValues.head

      val colPosn = new CollimatorPosition(None, extendedData.output.outputPK.get, status.toString, uid, beamName,
        scaledEdges.north,
        scaledEdges.south,
        scaledEdges.east,
        scaledEdges.west,
        planMinusScaled.north,
        planMinusScaled.south,
        planMinusScaled.east,
        planMinusScaled.west,
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
      val resultList = Config.CollimatorPositionBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName)).map(beamName => measureImage(beamName, extendedData, runReq))

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
