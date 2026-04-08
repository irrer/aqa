package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  *
  */
case class FindPeakAndValleysPrecisely() extends Logging {

  private case class PrecisePeakOrValley(coarsePeakValley: PeakOrValley, preciseCenter: Double, isPeak: Boolean, isWide: Boolean) {}
  // private case class PrecisePeakOrValley(preciseCenter: Double, isPeak: Boolean, isWide: Boolean) {}

  /**
    * Convert a coarse measurement into a precise one.
    * Make a list of all profile values between the adjacent peaks or valleys and use this sub-profile to
    * calculate a center of mass.  If this is a valley, swap the high and low values and then calculate
    * the center of mass.
    *
    * @param profile Entier profile (sums of rows of pixels).
    * @param pvList  List of coarse measurements
    * @param index   Index of coarse peak or valley.  There must be an adjacent pair of either peaks or
    *                valleys to either side, so this can not be the first or last index.
    * @return
    */
  private def coarseToPrecise(profile: Seq[Float], pvList: Seq[PeakOrValley], index: Int): PrecisePeakOrValley = {
    val pv = pvList(index)

    // Make a list of all profile values between the adjacent peaks or valleys and use this sub-profile to
    // calculate a center of mass.  If this is a valley, swap the high and low values and then calculate
    // the center of mass.
    val precise = {

      def reverseMass(valueList: Seq[Float]): Seq[Float] = {
        val max = valueList.max
        valueList.map(v => max - v)
      }

      val lo = pvList(index - 1).center.round.toInt
      val hi = pvList(index + 1).center.round.toInt
      val width = hi - lo
      val valueList = {
        val list = profile.slice(lo, lo + width)
        val min = list.min
        val minToZero = list.map(_ - min)
        if (pv.isPeak)
          minToZero
        else
          reverseMass(minToZero)
      }

      val centerOfMass = ImageUtil.centerOfMass(valueList.toIndexedSeq) + lo
      centerOfMass
    }

    PrecisePeakOrValley(pv, precise, pv.isPeak, pv.isWide)
  }

  def findPeakAndValleysPrecisely(profile: Seq[Float]): Seq[Double] = {

    val pvList = FindPeakAndValleyCoarsely().findPeaksAndValleysCoarsely(profile)

    val preciseList = pvList.indices.drop(1).dropRight(1).map(index => coarseToPrecise(profile, pvList, index))

    def toBoundary(index: Int): Option[Double] = {

      val boundary = {
        if (index > 0) {
          val prev = preciseList(index - 1)
          val pv = preciseList(index)
          if (prev.isWide == pv.isWide) {
            Some((prev.preciseCenter + pv.preciseCenter) / 2)
          } else {
            val next = preciseList(index + 1)
            if (next.isWide == pv.isWide) {
              val boundary = pv.preciseCenter - ((next.preciseCenter - pv.preciseCenter) / 2)
              Some(boundary)
            } else {
              logger.warn("Unexpected - boundary not found.")
              None
            }
          }
        } else None
      }

      boundary
    }

    preciseList.indices.flatMap(toBoundary)

    if (preciseList.size > 1) {
      val borderList = preciseList.indices.tail.map(index => (preciseList(index).preciseCenter + preciseList(index - 1).preciseCenter) / 2)
      val widthList = borderList.indices.tail.map(index => borderList(index) - borderList(index - 1))
      Trace.trace(s"widthList:\n${widthList.mkString("\n")}")
      Trace.showChart(widthList.drop(3).dropRight(5), "Precise Width List")
      if (true) {
        val evenlyList = ImageUtil.evenlySpacedLeastSquares(borderList.drop(5).dropRight(5))
        evenlyList
      } else {
        borderList
      }
    } else
      Seq()
  }

}
