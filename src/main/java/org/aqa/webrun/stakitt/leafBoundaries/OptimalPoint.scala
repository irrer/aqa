package org.aqa.webrun.stakitt.leafBoundaries

import org.aqa.Logging
import org.opensourcephysics.numerics.CubicSpline

import scala.annotation.tailrec

/**
  * Find the best point within a range.
  */
object OptimalPoint extends Logging {

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
  def optimalPoint(initialLo: Double, initialHi: Double, cs: CubicSpline, rate: Double => Double): Double = {

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


}
