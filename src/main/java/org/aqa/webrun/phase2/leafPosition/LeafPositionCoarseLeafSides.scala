package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.db.LeafPosition
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import java.awt.geom.Point2D
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.LocateRidge
import java.awt.geom.Rectangle2D
import java.awt.Rectangle
import edu.umro.ScalaUtil.Trace
import edu.umro.ImageUtil.ImageUtil

object LeafPositionCoarseLeafSides extends Logging {

  /**
   * Get a list of the coarsely (to the nearest pixel) located sides of the collimator leaves.  Values are in pixels and are ordered by position.
   *
   * The general approach is to take the profile of the entire image and look for the peaks and valleys formed by the side edges of the leaves.
   */
  def coarseLeafSides(horizontal: Boolean, profile: IndexedSeq[Float], attributeList: AttributeList, minLeafWidth_mm: Double, maxLeafWidth_mm: Double, dicomImage: DicomImage): Seq[Double] = {

    // used when considering how precisely leaves need to be spaced to determine whether ridges are classified as leaf sides or not.
    val marginOfError_pct = 15.0

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

    // leaves must be at least this wide to be considered leaves
    val minLeafWidth_pix = {
      val m = if (horizontal) translator.iso2PixDistY(minLeafWidth_mm) else translator.iso2PixDistX(minLeafWidth_mm)
      m / (1 + marginOfError_pct / 100)
    }

    // leaves must be no wider than this to be considered leaves
    val maxLeafWidth_pix = {
      val m = if (horizontal) translator.iso2PixDistY(maxLeafWidth_mm) else translator.iso2PixDistX(maxLeafWidth_mm)
      m * (1 + marginOfError_pct / 100)
    }

    val searchDistance_pix = (minLeafWidth_pix / 3).round.toInt
    Trace.trace("searchDistance_pix: " + searchDistance_pix)

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

    // start evaluating at the middle of the image and search in both directions for the extent of the
    // valid leaves.  Note the double reverse for the lower half of peak/valleys.
    val validValleyPeakList = dropAfterLastPeak(lo.reverse).reverse ++ dropAfterLastPeak(hi)

    // filter to only get peaks, and convert to Double's
    val peakList = validValleyPeakList.filter(pv => pv.shape == Shape.peak).map(pv => pv.position.toDouble)
    peakList
  }

}
