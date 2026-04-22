package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Logging
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.Config

import java.awt.geom.Rectangle2D

case class StakittAOI(xIndex: Int, yIndex: Int, rectangle: Rectangle2D.Double, plannedXEdge_mm: Double) extends Logging {
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

  /** List of how much each row of pixels contributes to the leaf end position. */
  private val yWeightList: Seq[Double] = yCoordinateList.map(toWeight)

  /**
    * Measures the position of the leaf's end in absolute (not relative) pixels.
    * @param leafEndBySinglePixel List of all measured leaf ends for each row of pixels.
    * @return End of leaf in absolute pixels.
    */
  def measureLeafEnd(leafEndBySinglePixel: LeafEndBySinglePixel): Double = {

    // Edge position for each individual row of pixels in the leaf.
    val pixelWiseEdgeList_pix = yCoordinateList.map(y_pix => leafEndBySinglePixel.get(rectangle.x, y_pix))

    val xPosition_pix = {
      // the sum of all the edge positions of the individual rows of pixels, with the top and bottom rows weighted in proportion
      // to their contribution of the edge.  So for example if only 30% a row of pixels is in the AOI, then multiply that row's
      // edge by 0.30
      val xPosition_sum = {
        val list = pixelWiseEdgeList_pix.zip(yWeightList).map(vw => vw._1 * vw._2)
        list.sum
      }

      // divide the sum of positions by the total height of the rectangle to get the mean position of the leaf end
      xPosition_sum / rectangle.height
    }

    xPosition_pix
  }

}

object StakittAOI extends Logging {

  def makeAOIs(approximateLeafEnds: Seq[Double], yImageBorders: LeafBoundaries, rtimage: AttributeList, plannedXEdgeList: Seq[Double]): Seq[StakittAOI] = {

    val trans = new IsoImagePlaneTranslator(rtimage)

    // abbreviation
    type Rect2d = Rectangle2D.Double

    val x0 = approximateLeafEnds.head
    val x1 = approximateLeafEnds(1)
    val x2 = approximateLeafEnds(2)

    val xL0 = approximateLeafEnds.last

    val xFilled = x2 - x1

    val xAoiPairList = XAoiBorders.makeXPairList(approximateLeafEnds)

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
    def makeAOIWithYMargin_pix(xIndex: Int, yIndex: Int): StakittAOI = {

      val verticalMargin_pix = trans.iso2PixDistY(Config.StakittVerticalMargin_mm)

      val rect = makeAOI(xIndex, yIndex)
      val rectWithMargin = new Rect2d(rect.x, rect.y + verticalMargin_pix, rect.width, rect.height - (2 * verticalMargin_pix))
      StakittAOI(xIndex, yIndex, rectWithMargin, plannedXEdgeList(xIndex))
    }

    def makeColumn(xIndex: Int): Seq[StakittAOI] = {
      yImageBorders.yPointListLo_pix.adjusted_pix.indices.dropRight(1).map(yIndex => makeAOIWithYMargin_pix(xIndex, yIndex))
    }

    val list = approximateLeafEnds.indices.flatMap(makeColumn)

    list
  }
}
