package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
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
case class FindBoundariesByMidpoints(image: DicomImage, name: String) extends Logging {

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
    * Given a profile of the square wave, determine where it crosses the mean value of the wave.
    *
    * @return List of leaf boundaries
    */
  private def findBoundariesByMidPoints(): Seq[Double] = {

    val borderList = pvList.indices.tail.map(findCrossingPoint)
    if (true) { // TODO rm
      val text = borderList.indices.tail.map(i => borderList(i) + " : " + (borderList(i) - borderList(i - 1))).mkString("\n")
      Trace.trace(s"Border list for $name\n$text\n")
    }
    borderList

  }

  def findLeafBoundaries(): Seq[Double] = {
    val measured = findBoundariesByMidPoints()

    val widthList = measured.indices.tail.map(i => measured(i) - measured(i - 1))

    val approximateNarrowWidth = widthList.sorted.take(5).sum / 5

    def isWide(width: Double): Boolean = width > (approximateNarrowWidth * 1.5)

    val topWideCount = widthList.takeWhile(w => isWide(w)).size

    val bottomWideCount = widthList.reverse.takeWhile(w => isWide(w)).size

    val evenlySpaced = {
      val topWideList = measured.take(topWideCount)
      val bottomWideList = measured.takeRight(bottomWideCount)

      def makeNarrow(index: Int): Option[Double] = {
        val width = measured(index + 1) - measured(index)
        if (isWide(width)) {
          val middle = (measured(index + 1) + measured(index)) / 2
          Some(middle)
        } else
          None
      }

      val fakeBoundaries = measured.indices.dropRight(1).flatMap(makeNarrow)

      val measuredWithFakeBoundaries = (measured ++ fakeBoundaries).sorted

      val evenlySpaced = ImageUtil.evenlySpacedLeastSquares(measuredWithFakeBoundaries)

      val indicesToRemove = {
        val top = (0 until topWideCount).map(i => (i * 2) + 1)
        val bottom = (0 until bottomWideCount).map(i => evenlySpaced.size - ((i + 1) * 2))
        top ++ bottom
      }

      val measuredMadeEven = evenlySpaced.indices.filterNot(i => indicesToRemove.contains(i)).map(i => evenlySpaced(i))
      measuredMadeEven
    }

    val verticalBounds = findVerticalBoundaries()

    val wideWidth = evenlySpaced(1) - evenlySpaced.head

    val topList = {
      val count = ((evenlySpaced.head - verticalBounds._1) / wideWidth).round.toInt
      (0 until count).map(i => evenlySpaced.head - ((i + 1) * wideWidth))
    }

    val bottomList = {
      val count = ((verticalBounds._2 - evenlySpaced.last) / wideWidth).round.toInt
      (0 until count).map(i => evenlySpaced.last + ((i + 1) * wideWidth))
    }

    val boundaryList = (evenlySpaced ++ topList ++ bottomList).sorted

    if (true) { // TODO
      val j0 = boundaryList.indices.tail.map(i => boundaryList(i) - boundaryList(i - 1)).distinct.sorted
      Trace.trace(s"j0:\n${j0.mkString("\n")}")
    }

    boundaryList
  }

}
