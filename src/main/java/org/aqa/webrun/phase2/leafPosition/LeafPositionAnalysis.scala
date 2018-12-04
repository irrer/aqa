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
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.LocateRidge
import java.awt.geom.Rectangle2D
import java.awt.Rectangle
import edu.umro.ScalaUtil.Trace

object LeafPositionAnalysis extends Logging {

  case class LeafPositionResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[LeafPosition]) extends SubProcedureResult(sum, sts, subProcedureName)

  val subProcedureName = "Leaf Position"

  /**
   * Get a list of the coarsely (to the nearest pixel) located sides of the collimator leaves.  Values are in pixels and are ordered by position.
   *
   * The general approach is to take the profile of the entire image and look for the peaks and valleys formed by the side edges of the leaves.
   */
  private def coarseLeafSides(horizontal: Boolean, attributeList: AttributeList, minLeafWidth_mm: Double, maxLeafWidth_mm: Double, dicomImage: DicomImage): Seq[Double] = {

    // used when considering how precisely leaves need to be spaced to determine whether ridges are classified as leaf sides or not.
    val marginOfError_pct = 15.0

    val profile = if (horizontal) dicomImage.columnSums else dicomImage.rowSums
    val translator = new IsoImagePlaneTranslator(attributeList)

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

    // leaves must be at least this wide to be considered leaves
    val minLeafWidth_pix = {
      val m = if (horizontal) translator.iso2PixY(minLeafWidth_mm) else translator.iso2PixX(minLeafWidth_mm)
      m / (1 + marginOfError_pct / 100)
    }

    // leaves must be no widerd than this to be considered leaves
    val maxLeafWidth_pix = {
      val m = if (horizontal) translator.iso2PixY(maxLeafWidth_mm) else translator.iso2PixX(maxLeafWidth_mm)
      m * (1 + marginOfError_pct / 100)
    }

    val searchDistance_pix = (minLeafWidth_pix / 3).round.toInt
    println("distance: " + searchDistance_pix)

    /**
     * Return true if the given profile point is smaller than any of its neighbors within the search distance.
     */
    def isValley(pos: Int): Boolean = {
      (pos > searchDistance_pix) &&
        (pos < profile.size - searchDistance_pix) &&
        (profile(pos) != profile(pos + 1)) && // handle the highly unlikely but possible case where there are two (or more) consecutive points at the bottom of the valley with the same value.
        (pos - searchDistance_pix until pos + searchDistance_pix).map(p => profile(p) >= profile(pos)).reduce(_ && _)
    }

    /**
     * Return true if the given profile point is larger than any of its neighbors within the search distance.
     */
    def isPeak(pos: Int): Boolean = {
      (pos > searchDistance_pix) &&
        (pos < profile.size - searchDistance_pix) &&
        (profile(pos) != profile(pos + 1)) && // handle the highly unlikely but possible case where there are two (or more) consecutive points at the top of the peak with the same value.
        (pos - searchDistance_pix until pos + searchDistance_pix).map(p => profile(pos) >= profile(p)).reduce(_ && _)
    }

    def classify(pos: Int): PosVal = {
      0 match {
        case _ if isPeak(pos) => new PosVal(pos, profile(pos), Shape.peak)
        case _ if isValley(pos) => new PosVal(pos, profile(pos), Shape.valley)
        case _ => new PosVal(pos, profile(pos), Shape.flat)
      }
    }

    // classify all points as to their shape
    val classified = (searchDistance_pix until profile.size - searchDistance_pix).map(pos => classify(pos))

    // make list of only peaks and valleys
    val peakValleyList = classified.filter(posVal => posVal.shape != Shape.flat).sortBy(_.position)

    /**
     * Trim the list and remove any peaks and valleys at the end that do not meet the criteria for
     * leaf edge.  Points should alternate between peak and valley, ending in a peak.  They should
     * also not be too close together or too far apart.
     */
    def dropAfterLastPeak(list: IndexedSeq[PosVal]): IndexedSeq[PosVal] = {

      // decide whether 3 consecutive points describe two leaf side separated by a valley, or just noise.
      def getMore(pos: Int) = {
        def width = (list(pos).position - list(pos + 2).position).abs
        ((pos + 3) <= list.size) &&
          (list(pos).shape == Shape.peak) &&
          (list(pos + 1).shape == Shape.valley) &&
          (list(pos + 2).shape == Shape.peak) &&
          (width > minLeafWidth_pix) &&
          (width < maxLeafWidth_pix)
      }

      def search(pos: Int): Int = {
        if (getMore(pos)) search(pos + 2)
        else pos
      }

      val start = list.indexWhere(posVal => posVal.shape == Shape.peak)
      val lastPeak = search(start)
      list.take(lastPeak + 1)
    }

    val half = peakValleyList.size / 2 // roughly half of the values
    val lo = peakValleyList.take(half) // lower half
    val hi = peakValleyList.drop(half) // upper half

    println("lo:\n    " + lo.mkString("\n    "))
    println("hi:\n    " + hi.mkString("\n    "))

    // start evaluating at the middle of the image and search in both directions for the extent of the
    // valid leaves.  Note the double reverse for the lower half of peak/valleys.
    val validValleyPeakList = dropAfterLastPeak(lo.reverse).reverse ++ dropAfterLastPeak(hi)
    println("validValleyPeakList:\n    " + validValleyPeakList.mkString("\n    "))

    // filter to only get peaks, and convert to Double's
    validValleyPeakList.filter(pv => pv.shape == Shape.peak).map(pv => pv.position.toDouble)
  }

  /**
   * Get a sorted list of all the distinct leaf ends in mm as isoplane coordinates.
   */
  private def leafEnds(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
    val ControlPointSequence = Util.seq2Attr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagFromName.ControlPointSequence)
    val withEnergy = ControlPointSequence.filter(cp => cp.get(TagFromName.CumulativeMetersetWeight).getDoubleValues.head != 0)
    val BeamLimitingDevicePositionSequence = withEnergy.map(cps => Util.seq2Attr(cps, TagFromName.BeamLimitingDevicePositionSequence)).flatten

    def isMlc(BeamLimitingDevicePosition: AttributeList): Boolean = {
      val deviceType = BeamLimitingDevicePosition.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString
      val requiredType = if (horizontal) "MLCX" else "MLCY"
      deviceType.equalsIgnoreCase(requiredType)
    }

    val endList = BeamLimitingDevicePositionSequence.filter(bldp => isMlc(bldp)).map(bldp => bldp.get(TagFromName.LeafJawPositions).getDoubleValues.head).distinct.sorted
    endList
  }

  /**
   * Get the sorted, distinct leaf position boundaries (positions of sides of leaves) from the plan for this beam in isoplane mm.
   */
  private def LeafPositionBoundaries(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
    val BeamLimitingDeviceSequence = Util.seq2Attr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagFromName.BeamLimitingDeviceSequence)
    //val BeamLimitingDevicePositionSequence = BeamLimitingDeviceSequence.map(cps => Util.seq2Attr(cps, TagFromName.BeamLimitingDevicePositionSequence)).flatten
    def getLeafPositionBoundaries(bldps: AttributeList): Seq[Double] = {
      Trace.trace("\n" + bldps)
      val at = bldps.get(TagFromName.LeafPositionBoundaries)
      if (at == null)
        Seq[Double]()
      else
        at.getDoubleValues.toSeq
    }
    val LeafPositionBoundaries = BeamLimitingDeviceSequence.map(bldps => getLeafPositionBoundaries(bldps)).flatten.distinct.sorted
    LeafPositionBoundaries
  }

  /**
   * Hook for testing
   */
  def testLeafEnds(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = leafEnds(horizontal, beamName, plan)

  private def leafSides(horizontal: Boolean, beamName: String, imageAttrList: AttributeList, dicomImage: DicomImage, plan: AttributeList): Seq[Double] = {

    val leafSides_mm = LeafPositionBoundaries(horizontal, beamName, plan)
    val leafWidthList_mm = leafSides_mm.dropRight(1).zip(leafSides_mm.drop(1)).map(ab => (ab._1 - ab._2).abs).distinct.sorted
    val minLeafWidth_mm = leafWidthList_mm.head
    val maxLeafWidth_mm = leafWidthList_mm.last

    val coarse = coarseLeafSides(horizontal, imageAttrList, minLeafWidth_mm, maxLeafWidth_mm, dicomImage)

    val translator = new IsoImagePlaneTranslator(imageAttrList)
    val leafEndList_mm = LeafPositionBoundaries(horizontal, beamName, plan)
    val leafEndList_pix = leafEndList_mm.map(le => if (horizontal) translator.iso2PixX(le) else translator.iso2PixY(le))
    val searchDistance = (if (horizontal) translator.iso2PixY(minLeafWidth_mm) else translator.iso2PixX(minLeafWidth_mm)) / 2
    val minEnd = leafEndList_pix.min - searchDistance
    val maxEnd = leafEndList_pix.max + searchDistance
    val pixelArray = dicomImage.getSubArray(new Rectangle(0, 0, dicomImage.width, dicomImage.height))

    def locateRidge(leafSide: Double): Double = {
      if (horizontal) {
        val width = maxEnd - minEnd
        val boundingRectangle = new Rectangle2D.Double(minEnd, leafSide - searchDistance, width, searchDistance * 2)
        val ridge = LocateRidge.locateVertical(pixelArray, boundingRectangle)
        ridge
      } else {
        val height = maxEnd - minEnd
        val boundingRectangle = new Rectangle2D.Double(leafSide - searchDistance, minEnd, searchDistance * 2, height)
        val ridge = LocateRidge.locateVertical(pixelArray, boundingRectangle)
        ridge
      }
    }

    val precise = leafSides_mm.map(ls => locateRidge(ls))

    precise
  }

  /**
   * Hook for testing
   */
  def testLeafSides(horizontal: Boolean, beamName: String, imageAttrList: AttributeList, dicomImage: DicomImage, plan: AttributeList): Seq[Double] =
    leafSides(horizontal, beamName, imageAttrList, dicomImage, plan)

  private def measureBeam(beamName: String, outputPK: Long, imageAttrList: AttributeList, image: DicomImage, plan: AttributeList): LeafPosition = {

    // true if collimator is horizontal
    val horizontal = Phase2Util.isHorizontal(imageAttrList)
    val translator = new IsoImagePlaneTranslator(imageAttrList)

    val leafEndList = leafEnds(horizontal, beamName, plan)
    ???
  }

  /**
   * Hook for testing
   */
  def testMeasureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage, plan: AttributeList): LeafPosition =
    measureBeam(beamName, outputPK, attributeList, image, plan)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, LeafPositionResult] = {
    ???
  }

}