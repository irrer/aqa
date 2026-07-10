package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging
import org.opensourcephysics.numerics.CubicSpline

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  *
  * @param imageOfStaggeredLeaves Use this image. It will either be  the left-hand or right-hand part of the whole image.
  * @param name Used for diagnostics and debugging.
  */
case class FindLeafBoundaries(imageOfStaggeredLeaves: DicomImage, name: String, coarseVerticalFieldExtent: CoarseVerticalFieldExtent) extends Logging {

  /** Scale the profile to values 0 to 100 to make debugging easier. */
  private val profileScaled = {
    val profileRaw: Seq[Float] = imageOfStaggeredLeaves.rowSums
    val min = profileRaw.min
    val max = profileRaw.max
    val range = (max - min) / 100
    profileRaw.map(y => (y - min) / range)
  }

  /** Cubic spline for profile. */
  private val cs = new CubicSpline(profileScaled.indices.map(_.toDouble).toArray, profileScaled.map(_.toDouble).toArray)

  /** List of approximate locations of peaks and valleys. */
  private val pvList = FindPeakAndValleyCoarsely.findPeaksAndValleysCoarsely(imageOfStaggeredLeaves, coarseVerticalFieldExtent)

  /**
    * Given a coarsely located peak or valley, find it precisely.
    * @param pv Peak or valley.
    * @return Precise center.
    */
  private def findPeakOrValley(pv: PeakOrValley): Double = {
    val eval: Double => Double = {
      if (pv.isPeak)
        x => x // if peak, find highest
      else
        x => -x // if valley, find lowest
    }
    OptimalPoint.optimalPoint(pv.indexes.head, pv.indexes.last, cs, eval)
  }

  /**
    * Find the leaf boundary preceding this peak or valley.
    *
    * Do this by locating where the profile crosses the mean between the given peak or valley and its predecessor.
    *
    * @param index Index of peak or valley.
    * @return Crossing point, which is the position of the leaf boundary.
    */
  private def findCrossingPoint(index: Int): Double = {

    // establish a search range
    val lo: Double = findPeakOrValley(pvList(index - 1))
    val hi: Double = findPeakOrValley(pvList(index))

    // the midpoint between a pair of adjacent peak and valley.
    val mean = (cs.evaluate(hi) + cs.evaluate(lo)) / 2

    /**
      * Determine how far away a value is from the mean.
      * @param yValue The results of a guess: <code>cs.evaluate(guess)</code>
      * @return Rating for how good the point is.  Bigger is better.
      */
    def rate(yValue: Double): Double = {
      val v = (mean - yValue).abs
      -v
    }

    val x = OptimalPoint.optimalPoint(lo, hi, cs, rate)

    x
  }

  /**
    * Add the boundaries to the top and bottom that, because of their position, are not easily measured by
    * looking at their crossing point.
    *
    * This is done by finding the vertical extent of the field, extrapolating how many more boundaries are
    * needed, and then add them at the regular spacing points.
    *
    * @param evenlySpaced List of evenly spaced boundaries.
    * @return New list with top and bottom boundaries added.
    */
  private def addExtrapolatedBoundaries(evenlySpaced: Seq[Double]): Seq[Double] = {
    // val verticalBounds = findVerticalBoundaries()
    // val verticalFieldExtent = VerticalFieldExtent(imageOfStaggeredLeaves)
    val top_pix = coarseVerticalFieldExtent.top_pix
    val bottom_pix = coarseVerticalFieldExtent.bottom_pix

    val wideWidth = evenlySpaced(1) - evenlySpaced.head

    def topList = {
      val count = ((evenlySpaced.head - top_pix) / wideWidth).round.toInt
      (0 until count).map(i => evenlySpaced.head - ((i + 1) * wideWidth))
    }

    def bottomList = {
      val count = ((bottom_pix - evenlySpaced.last) / wideWidth).round.toInt
      (0 until count).map(i => evenlySpaced.last + ((i + 1) * wideWidth))
    }

    val boundaryList = (evenlySpaced ++ topList ++ bottomList).sorted

    boundaryList
  }

  /**
    * Find the leaf boundaries (sides).  Return them as a list of pixel coordinates in the Y axis.
    *
    * @return List of leaf boundaries.
    */
  def findLeafBoundaries_pix(): LeafBoundariesMeasuredAndAdjusted = {
    val measured_pix = pvList.indices.tail.map(findCrossingPoint)
    val measuredWithExtrapolated_pix = addExtrapolatedBoundaries(measured_pix)

    val adjustedSpacing_pix = HomogenizeBoundaries.homogenize(measured_pix)

    if (adjustedSpacing_pix.size != measured_pix.size)
      throw new RuntimeException(s"Started with ${measured_pix.size} boundaries but number of evenly spaced is ${adjustedSpacing_pix.size}")

    val boundaryList_pix = addExtrapolatedBoundaries(adjustedSpacing_pix)

    val widths = boundaryList_pix.indices.tail.map(i => boundaryList_pix(i) - boundaryList_pix(i - 1))
    if (widths.min < 1)
      throw new RuntimeException(s"Bad width between boundaries found of ${widths.min}")

    logger.info(s"Number of leaf boundaries found for $name: ${boundaryList_pix.size}    min_pix: ${widths.min}    max_pix: ${widths.max}")

    LeafBoundariesMeasuredAndAdjusted(measuredWithExtrapolated_pix, boundaryList_pix)
  }

}
