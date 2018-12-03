package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.db.LeafPosition
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import java.awt.geom.Point2D
import org.aqa.webrun.phase2.MeasureTBLREdges

object LeafPositionAnalysis extends Logging {

  case class LeafPositionResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[LeafPosition]) extends SubProcedureResult(sum, sts, subProcedureName)

  val subProcedureName = "Leaf Position"

  /**
   * Get a list of the precisely located sides of the collimator leaves.  Values are in pixels and are ordered by position.
   */
  private def leafSides(horizontal: Boolean, attributeList: AttributeList, dicomImage: DicomImage): Seq[Double] = {

    val profile = if (horizontal) dicomImage.columnSums else dicomImage.rowSums
    val translator = new IsoImagePlaneTranslator(attributeList)

    // distance in pixels from the furthest sides of the leaves
    val leafSideRange = {
      val x1x2y1y2 = MeasureTBLREdges.imageCollimatorPositions(attributeList)
      val point1 = translator.iso2Pix(new Point2D.Double(x1x2y1y2.X1, x1x2y1y2.Y1))
      val point2 = translator.iso2Pix(new Point2D.Double(x1x2y1y2.X2, x1x2y1y2.Y2))
      val pair = if (horizontal) Seq(point1.getY, point2.getY) else Seq(point1.getX, point2.getX)
      pair.sorted
    }

    object Shape extends Enumeration {
      val peak = Value
      val valley = Value
      val flat = Value
    }

    case class PosVal(position: Int, value: Float, shape: Shape.Value) {
      def this(position: Int, value: Float) = this(position, value, Shape.flat)
      def this(pv: PosVal, shape: Shape.Value) = this(pv.position, pv.value, shape)
      override def toString = "pos: " + position.formatted("%4d") + "  value: " + value.formatted("%7.0f") + "   shape: " + shape.toString.format("%-6s")
    }

    val posValList = profile.zipWithIndex.map(pi => new PosVal(pi._2, pi._1))
    println("posValList:\n    " + posValList.mkString("\n    "))

    val minLeafWidthInPixels = 22.32142857142857

    val distance = (minLeafWidthInPixels / 3).round.toInt
    println("distance: " + distance)

    def isValley(pos: Int) = {
      (pos > distance) &&
        (pos < profile.size - distance) &&
        (pos - distance until pos + distance).map(p => profile(p) >= profile(pos)).reduce(_ && _)
    }

    def isPeak(pos: Int) = {
      (pos > distance) &&
        (pos < profile.size - distance) &&
        (pos - distance until pos + distance).map(p => profile(pos) >= profile(p)).reduce(_ && _)
    }

    def classify(pos: Int): PosVal = {
      0 match {
        case _ if isPeak(pos) => new PosVal(pos, profile(pos), Shape.peak)
        case _ if isValley(pos) => new PosVal(pos, profile(pos), Shape.valley)
        case _ => new PosVal(pos, profile(pos), Shape.flat)
      }
    }

    val classified = (distance until profile.size - distance).map(pos => classify(pos))

    val peakValleyList = classified.filter(posVal => posVal.shape != Shape.flat).sortBy(_.position)

    def dropAfterLastPeak(list: IndexedSeq[PosVal]): IndexedSeq[PosVal] = {

      def getMore(pos: Int) = {
        ((pos + 3) <= list.size) &&
          (list(pos).shape == Shape.peak) &&
          (list(pos + 1).shape == Shape.valley) &&
          (list(pos + 2).shape == Shape.peak)
      }

      def search(pos: Int): Int = {
        if (getMore(pos)) search(pos + 2)
        else pos
      }

      val start = list.indexWhere(posVal => posVal.shape == Shape.peak)
      val lastPeak = search(start)
      list.take(lastPeak + 1)
    }

    val half = peakValleyList.size / 2
    val lo = peakValleyList.take(half)
    val hi = peakValleyList.drop(half)

    println("lo:\n    " + lo.mkString("\n    "))
    println("hi:\n    " + hi.mkString("\n    "))

    val validValleyPeakList = dropAfterLastPeak(lo.reverse).reverse ++ dropAfterLastPeak(hi)
    println("validValleyPeakList:\n    " + validValleyPeakList.mkString("\n    "))

    // TODO continue here
    //   - localized enter of mass?
    //   - LocateRidge for each leaf side

    ???
  }

  private def measureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage): LeafPosition = {

    // true if collimator is horizontal
    val horizontal = (Util.angleRoundedTo90(attributeList.get(TagFromName.BeamLimitingDeviceAngle).getDoubleValues.head).toInt % 180) == 0

    ???
  }

  /**
   * Hook for testing
   */
  def testMeasureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage): LeafPosition = measureBeam(beamName, outputPK, attributeList, image)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, LeafPositionResult] = {
    ???
  }

}