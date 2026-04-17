package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.Config

import java.awt.geom.Rectangle2D

case class StakittAOI(xIndex: Int, yIndex: Int, rectangle: Rectangle2D.Double) {
  val firstPixelRowY: Int = rectangle.y.floor.round.toInt

  private val bottom = rectangle.y + rectangle.height

  val yCoordinateList: Seq[Int] = (rectangle.y.floor.round.toInt until bottom.ceil.round.toInt).map(i => i)

  private def toWeight(y: Int): Double = {
    y match {
      case _ if y == yCoordinateList.head => rectangle.y.ceil - rectangle.y
      case _ if y == yCoordinateList.last => bottom - bottom.floor
      case _                              => 1.0
    }
  }

  val yWeightList: Seq[Double] = yCoordinateList.map(toWeight)

  if (true) {
    val sum = yWeightList.sum
    val diff = (sum - rectangle.height).abs
    val err = diff / rectangle.height
    if (err > 0.0000000001) {
      Trace.trace(s"Y weights do not match height: sum: $sum    height: ${rectangle.height}    diff: $diff    " + this)
      Trace.trace()
    }
  }
}

object StakittAOI extends Logging {

  private def mean(d1: Double, d2: Double) = (d1 + d2) / 2

  private case class XAoiBorders(lo: Double, hi: Double) {
    val mid: Double = mean(lo, hi)
    val width: Double = hi - lo
  }

  private def makeXPairList(xList: Seq[Double]): Seq[XAoiBorders] = {

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

  def makeAOIs(xImageBorders: LeafEnds, yImageBorders: LeafBoundaries, rtimage: AttributeList): Seq[StakittAOI] = {

    val trans = new IsoImagePlaneTranslator(rtimage)

    // abbreviation
    type Rect2d = Rectangle2D.Double

    val xList = xImageBorders.xPointList

    val x0 = xList.head
    val x1 = xList(1)
    val x2 = xList(2)

    val xL0 = xList.last

    val xFilled = x2 - x1

    val xAoiPairList = makeXPairList(xImageBorders.xPointList)

    val xMin = x0 - xFilled
    val xMax = xL0 + xFilled
    val xRange = xMax - xMin

    val yRange = yImageBorders.yPointListLo_pix.adjusted_pix.head - yImageBorders.yPointListHi_pix.adjusted_pix.head

    def makeAOI(xIndex: Int, yIndex: Int): Rect2d = {

      val xAoi = xAoiPairList(xIndex)

      val yDelta = yRange * (xAoi.mid / xRange)

      val y = yImageBorders.yPointListLo_pix.adjusted_pix(yIndex) - yDelta

      val yHeight = yImageBorders.yPointListLo_pix.adjusted_pix(yIndex + 1) - yImageBorders.yPointListLo_pix.adjusted_pix(yIndex)
      val rect = new Rect2d(xAoi.lo, y, xAoi.width, yHeight)
      rect
    }

    /**
      * Make an AOI with a top and bottom margin.
      *
      * @param xIndex X index of AOI array.
      * @param yIndex Y index of AOI array.
      * @return A rectangle in absolute pixel coordinates.
      */
    def makeAOIWithMargin_pix(xIndex: Int, yIndex: Int): StakittAOI = {

      val verticalMargin_pix = trans.iso2PixDistY(Config.StakittVerticalMargin_mm)

      val rect = makeAOI(xIndex, yIndex)
      val rectWithMargin = new Rect2d(rect.x, rect.y + verticalMargin_pix, rect.width, rect.height - (2 * verticalMargin_pix))
      StakittAOI(xIndex, yIndex, rectWithMargin)
    }

    def makeColumn(xIndex: Int): Seq[StakittAOI] = {
      yImageBorders.yPointListLo_pix.adjusted_pix.indices.dropRight(1).map(yIndex => makeAOIWithMargin_pix(xIndex, yIndex))
    }

    val list = xImageBorders.xPointList.indices.flatMap(makeColumn)

    list
  }
}
