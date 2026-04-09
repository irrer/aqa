package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Logging
import org.opensourcephysics.numerics.CubicSpline

import java.awt.Rectangle
import scala.annotation.tailrec

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  *
  * @param image Use this image. It will either be  the left-hand or right-hand part of the whole image.
  * @param name Used for diagnostics and debugging.
  */
case class FindLeafBoundaries(image: DicomImage, name: String) extends Logging {

  /** Scale the profile to values 0 to 100 to make debugging easier. */
  private val profileScaled = {
    val profileRaw: Seq[Float] = image.rowSums
    val min = profileRaw.min
    val max = profileRaw.max
    val range = (max - min) / 100
    profileRaw.map(y => (y - min) / range)
  }

  /** Cubic spline for profile. */
  private val cs = new CubicSpline(profileScaled.indices.map(_.toDouble).toArray, profileScaled.map(_.toDouble).toArray)

  /** List of approximate locations of peaks and valleys. */
  private val pvList = FindPeakAndValleyCoarsely().findPeaksAndValleysCoarsely(profileScaled)

  /**
    * Find the approximate upper and lower extents of the open part of the field.
    * @return X coordinates of top and bottom.
    */
  private def findVerticalBoundaries(): (Int, Int) = {
    val span: Int = 5

    val columnSums = image.columnSums

    val brightest = columnSums.indices.sortBy(i => columnSums(i)).takeRight(span)

    val brightestImage = image.getSubimage(new Rectangle(brightest.head, 0, brightest.last - brightest.head, image.Rows))

    val verticalProfile = brightestImage.rowSums

    val mid = (verticalProfile.max + verticalProfile.min) / 2

    val lo = verticalProfile.indexWhere(_ > mid)
    val hi = verticalProfile.lastIndexWhere(_ > mid)

    (lo, hi)
  }

  /**
    * Given a range and a rating function, find the best point on the cubic spline within that range.
    *
    * It is expected that the rating function and cubic spline are 'well-behaved', in that there must be a single
    * best point within the range, and that there are no false optimal points.
    *
    * @param initialLo Low end of range.
    * @param initialHi High end of range.
    * @param rate Function that will rate how 'good' the point is.  Bigger is better.  The function will be
    *             given the value: <code>cs.evaluate(guess)</code> and must return a rating.
    * @return
    */
  private def optimalPoint(initialLo: Double, initialHi: Double, rate: Double => Double): Double = {

    val maxRepetitions = 50 // stop after this many approximations
    val partitions = 5 // break the range into this many sub-regions.
    val indexRange = 0 until partitions

    @tailrec
    def opt(lo: Double, hi: Double, count: Int = maxRepetitions): Double = {

      val partitionSize = (hi - lo) / partitions

      val xList: Seq[Double] = indexRange.map(x => (x * partitionSize) + lo)
      val yList: Seq[Double] = xList.map(cs.evaluate)

      val bestIndex = indexRange.maxBy(index => rate(yList(index)))

      val newLo = ((bestIndex - 1) * partitionSize) + lo
      val newHi = ((bestIndex + 1) * partitionSize) + lo

      val best = (bestIndex * partitionSize) + lo

      // stop if count is exceeded or the range is zero.
      if ((count > 0) && ((newLo - newHi).abs > 0)) {
        opt(newLo, newHi, count - 1)
      } else
        best
    }

    opt(initialLo, initialHi)
  }

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
    optimalPoint(pv.indexes.head, pv.indexes.last, eval)
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

    val x = optimalPoint(lo, hi, rate)

    x
  }

  /**
    * Take the measured boundaries and make them evenly spaced.
    *
    * Some of them are double width, so first insert a fake, temporary boundary to make them all the same.
    *
    * Next, apply the least-squares algorithm to change from close to evenly spaced to be exactly evenly spaced.
    *
    *  Finally, remove the fake temporary boundaries and return the result.
    *
    * @param measured Measured boundaries.
    * @return Equivalent list as the measured parameters, but the position of each adjusted slightly so
    *         that they are evenly spaced.
    */
  private def makeEvenlySpaced(measured: Seq[Double]): Seq[Double] = {

    // all spaces between boundaries
    val widthList = measured.indices.tail.map(i => measured(i) - measured(i - 1))

    def isWide(width: Double): Boolean = {
      val approximateNarrowWidth = widthList.sorted.take(5).sum / 5
      width > (approximateNarrowWidth * 1.5)
    }

    /**
      * If this is the lower boundary of a wide leaf, make a fake temporary boundary to be positioned after
      * this one.  Otherwise, return None.
      *
      * @param index Index of a measured boundary.
      * @return New fake temporary boundary or None.
      */
    def makeNarrow(index: Int): Option[Double] = {
      val width = measured(index + 1) - measured(index)
      if (isWide(width)) {
        val middle = (measured(index + 1) + measured(index)) / 2
        Some(middle)
      } else
        None
    }

    // make all the fake boundaries needed
    val fakeBoundaries = measured.indices.dropRight(1).flatMap(makeNarrow)

    // merge the fakes into the real
    val measuredWithFakeBoundaries = (measured ++ fakeBoundaries).sorted

    // make an evenly spaced version
    val evenlySpaced = ImageUtil.evenlySpacedLeastSquares(measuredWithFakeBoundaries)

    // the list of indices of the fake boundaries
    val indicesToRemove = {
      val topWideCount = widthList.takeWhile(w => isWide(w)).size
      val top = (0 until topWideCount).map(i => (i * 2) + 1)

      val bottomWideCount = widthList.reverse.takeWhile(w => isWide(w)).size
      val bottom = (0 until bottomWideCount).map(i => evenlySpaced.size - ((i + 1) * 2))
      top ++ bottom
    }

    // remove the fake boundaries
    val measuredMadeEven = evenlySpaced.indices.filterNot(i => indicesToRemove.contains(i)).map(i => evenlySpaced(i))

    measuredMadeEven
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
    val verticalBounds = findVerticalBoundaries()

    val wideWidth = evenlySpaced(1) - evenlySpaced.head

    def topList = {
      val count = ((evenlySpaced.head - verticalBounds._1) / wideWidth).round.toInt
      (0 until count).map(i => evenlySpaced.head - ((i + 1) * wideWidth))
    }

    def bottomList = {
      val count = ((verticalBounds._2 - evenlySpaced.last) / wideWidth).round.toInt
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
  def findLeafBoundaries_pix(): Seq[Double] = {
    val measured = pvList.indices.tail.map(findCrossingPoint)

    val evenlySpaced = makeEvenlySpaced(measured)

    if (evenlySpaced.size != measured.size)
      throw new RuntimeException(s"Started with ${measured.size} boundaries but number of evenly spaced is ${evenlySpaced.size}")

    val boundaryList = addExtrapolatedBoundaries(evenlySpaced)

    val widths = boundaryList.indices.tail.map(i => boundaryList(i) - boundaryList(i - 1))
    if (widths.min < 1)
      throw new RuntimeException(s"Bad width between boundaries found of ${widths.min}")

    logger.info(s"Number of leaf boundaries found for $name: ${boundaryList.size}    min_pix: ${widths.min}    max_pix: ${widths.max}")

    boundaryList
  }

}
