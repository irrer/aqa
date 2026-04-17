package org.aqa.webrun.stakitt

import org.aqa.Logging

/**
 * Define a pair of borders vertical that delineate the AOIs around leaf ends.
 * @param lo Lower border (to the left).
 * @param hi Higher border (to the right).
 */
case class XAoiBorders(lo: Double, hi: Double) extends Logging {
  val mid: Double =  XAoiBorders mean(lo, hi)
  val width: Double = hi - lo
}

object XAoiBorders extends Logging {

  private def mean(d1: Double, d2: Double) = (d1 + d2) / 2

  /**
   * Construct a list of X borders.
   * @param xList List of approximations of leaf edges.
   * @return List of border pairs.
   */
  def makeXPairList(xList: Seq[Double]): Seq[XAoiBorders] = {

    val x0 = xList.head
    val x1 = xList(1)
    val x2 = xList(2)

    val xL0 = xList.last
    val xL1 = xList(xList.size - 2)
    val xL2 = xList(xList.size - 3)

    val xFilled = x2 - x1

    val head = {
      val lo = x0 - (xFilled / 2)
      val hi = mean(x0, x1)
      XAoiBorders(lo, hi)
    }

    def toXAoi(index: Int): XAoiBorders = {
      val lo = mean(xList(index - 1), xList(index))
      val hi = mean(xList(index), xList(index + 1))
      XAoiBorders(lo, hi)
    }

    val mid = xList.indices.drop(1).dropRight(1).map(toXAoi)

    val last = {
      val lo = xL0 - ((xL0 - xL1) / 2)
      val hi = xL0 + ((xL1 - xL2) / 2)
      XAoiBorders(lo, hi)
    }

    val all = head +: mid :+ last

    all
  }
}
