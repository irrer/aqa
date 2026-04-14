package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ImageUtil.ScaledImage
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.Stakitt
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.webrun.ExtendedData
import org.aqa.Util

import java.awt.geom.Rectangle2D
import java.awt.Color
import java.awt.image.BufferedImage
import scala.util.Random

case class LeafEndPositions(extendedData: ExtendedData, dicomImage: DicomImage, xImageBorders: LeafEnds, yImageBorders: LeafBoundaries, rtimage: AttributeList) extends Logging {

  private val trans = new IsoImagePlaneTranslator(rtimage)

  // abbreviation
  type Rect2d = Rectangle2D.Double

  private val xList = xImageBorders.xPointList

  private val x0 = xList.head
  private val x1 = xList(1)
  private val x2 = xList(2)

  private val xL0 = xList.last
  private val xL1 = xList(xList.size - 2)
  private val xL2 = xList(xList.size - 3)

  private val xFilled = x2 - x1

  private def mean(d1: Double, d2: Double) = (d1 + d2) / 2

  private case class XAoi(lo: Double, hi: Double) {
    // val loI: Int = lo.round.toInt
    // val hiI: Int = hi.round.toInt
    val mid: Double = mean(lo, hi)
    val width: Double = hi - lo
  }

  private val xAoiPairList: Seq[XAoi] = {

    val head = {
      val lo = x0 - (xFilled / 2)
      val hi = mean(x0, x1)
      XAoi(lo, hi)
    }

    def toXAoi(index: Int): XAoi = {
      val lo = mean(xList(index - 1), xList(index))
      val hi = mean(xList(index), xList(index + 1))
      XAoi(lo, hi)
    }

    val mid = xList.indices.drop(1).dropRight(1).map(toXAoi)

    val last = {
      val lo = xL0 - ((xL0 - xL1) / 2)
      val hi = xL0 + ((xL1 - xL2) / 2)
      XAoi(lo, hi)
    }

    val all = head +: mid :+ last
    all
  }

  private val xMin = x0 - xFilled
  private val xMax = xL0 + xFilled
  private val xRange = xMax - xMin

  private val yRange = yImageBorders.yPointListLo_pix.head - yImageBorders.yPointListHi_pix.head
  private val yHeight = mean( //
    yImageBorders.yPointListLo_pix(1) - yImageBorders.yPointListLo_pix.head,
    yImageBorders.yPointListHi_pix(1) - yImageBorders.yPointListHi_pix.head
  )

  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  val scale = 7
  val bufImg: BufferedImage = {
    val img = dicomImage.toDeepColorBufferedImage(0.01)
    ImageUtil.magnify(img, scale)
  }
  val gc = ImageUtil.getGraphics(bufImg)
  gc.setColor(Color.white)

  val rand = new Random()
  // val colorList = Seq(Color.white, Color.pink, Color.lightGray, Color.yellow)
  val colorList = Seq(Color.lightGray, Color.lightGray)

  val si = ScaledImage(scale, 0, 0)

  /*
  def vertLine(x: Double): Unit = si.drawLine(gc, x, 0, x, dicomImage.height)

  def horzLine(lineList: Seq[Double], name: String, offset: Int, color: Color): Unit = {
    gc.setColor(color)
    val rect = ImageText.getTextDimensions(gc, name)
    ImageText.drawTextCenteredAt(gc, (rect.getWidth / 2) + 20, (rect.getHeight + 1) * 2 * offset, name)
    lineList.foreach(y => si.drawLine(gc, 0, y, dicomImage.width, y))
  }
   */

  def drawRect(rect: Rect2d): Unit = {
    gc.setColor(colorList(rand.nextInt(colorList.size)))
    val x1 = rect.x
    val y1 = rect.y
    val x2 = x1 + rect.width
    val y2 = y1 + rect.height
    si.drawLine(gc, x1, y1, x2, y1)
    si.drawLine(gc, x1, y1, x1, y2)
    si.drawLine(gc, x1, y2, x2, y2)
    si.drawLine(gc, x2, y1, x2, y2)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  private def makeAOI(xIndex: Int, yIndex: Int): Rect2d = {

    val xAoi = xAoiPairList(xIndex)

    val yDelta = yRange * (xAoi.mid / xRange)

    val rect = new Rect2d(xAoi.lo, yImageBorders.yPointListLo_pix(yIndex) - yDelta, xAoi.width, yHeight)
    rect
  }

  private def findRowEdge(y: Int, xRange: Range): Double = {

    val minMaxSampleSize_pix = {
      val minMaxSampleSize_mm = 2.5 // TODO   Make configurable.
      trans.iso2PixDistX(minMaxSampleSize_mm).round.toInt
    }

    val list = xRange.map(x => dicomImage.get(x, y)) // one single row of pixes in the AOI
    val sorted = list.sorted
    val min = sorted.take(minMaxSampleSize_pix).sum / minMaxSampleSize_pix
    val max = sorted.takeRight(minMaxSampleSize_pix).sum / minMaxSampleSize_pix
    val midValue = mean(min, max)

    val e = LocateEdge.locateEdge(list, midValue)

    val xPosition_pix = xRange.head + e

    if (true) { // TODO rm
      gc.setColor(Color.white)
      Trace.trace(s"${xPosition_pix - 0.5}    $y    ${xPosition_pix + 0.5}    $y")
      si.drawLine(gc, xPosition_pix - 0.5, y, xPosition_pix + 0.5, y)
    }

    xPosition_pix
  }

  /**
    * Make an AOI with a top and bottom margin.
    * @param xIndex X index of AOI array.
    * @param yIndex Y index of AOI array.
    * @return A rectangle in absolute pixel coordinates.
    */
  private def makeAOIWithMargin_pix(xIndex: Int, yIndex: Int) = {

    val verticalMargin_pix = {
      val verticalMargin_mm = 0.2 // TODO ask Michael.  Make configurable.
      trans.iso2PixDistY(verticalMargin_mm)
    }
    val rect = makeAOI(xIndex, yIndex)
    drawRect(rect)
    new Rect2d(rect.x, rect.y + verticalMargin_pix, rect.width, rect.height - (2 * verticalMargin_pix))
  }

  /**
    * Measures the position of the leaf's end in absolute (not relative) pixels.
    * @param xIndex X index of AOI.
    * @param yIndex Y index of AOI.
    * @return End of leaf in absolute pixels.
    */
  private def measureLeafEnd(xIndex: Int, yIndex: Int): Double = { // StakittResult = { // TODO change type

    val rectWithMargin = makeAOIWithMargin_pix(xIndex, yIndex)

    val rectBottom = rectWithMargin.y + rectWithMargin.height

    val xRange: Range = rectWithMargin.x.floor.toInt until (rectWithMargin.x + rectWithMargin.width).ceil.toInt

    val xPosition_pix = {

      // the sum of all the edge positions of the individual rows of pixels, with the top and bottom rows weighted in proportion
      // to their contribution of the edge.  So for example if only 30% a row of pixels is in the AOI, then multiply that row's
      // edge by 0.30
      val xPosition_sum = {

        val headFraction = rectWithMargin.y.ceil - rectWithMargin.y
        val lastFraction = rectBottom - rectBottom.floor

        // Edge position for each individual row of pixels in the leaf.
        val pixelWiseEdgeList_pix = {
          // List of rows of pixels to measure for a single leaf.  Values are in absolute pixel coordinates.
          val yRange_pix = rectWithMargin.y.floor.toInt until rectBottom.ceil.toInt

          yRange_pix.map(y => findRowEdge(y, xRange))
        }

        // as a prelude to finding the mean, sum the edge coordinates, assigning each the appropriate weight.
        (headFraction * pixelWiseEdgeList_pix.head) + pixelWiseEdgeList_pix.drop(1).dropRight(1).sum + (lastFraction * pixelWiseEdgeList_pix.last)
      }

      // divide the sum of positions by the total height of the rectangle to get the mean position of the leaf end
      xPosition_sum / rectWithMargin.height
    }

    gc.setColor(Color.black)
    si.drawLine(gc, xPosition_pix, rectWithMargin.y, xPosition_pix, rectWithMargin.y + rectWithMargin.height)

    Trace.trace(s"Leaf end: $xPosition_pix")
    xPosition_pix
  }

  private def constructStakittResult(xIndex: Int, yIndex: Int): StakittResult = {
    val edgePosition_pix = measureLeafEnd(xIndex, yIndex)
    val edgePosition_mm = trans.pix2IsoCoordX(edgePosition_pix)
    val rect = makeAOI(xIndex, yIndex)

    val stakitt = Stakitt( //
      stakittPK = None,
      outputPK = extendedData.outputPK,
      SOPInstanceUID = Util.sopOfAl(rtimage),
      beamName = "NA", // TODO
      leafIndex = yIndex + 1,
      leafPositionIndex = xIndex + 1,
      measuredEndPosition_mm = edgePosition_mm,
      plannedEndPosition_mm = -1, // TODO
      measuredMinorSide_mm = trans.pix2IsoCoordY(rect.y),
      measuredMajorSide_mm = trans.pix2IsoCoordY(rect.y + rect.height)
    )

    val result = StakittResult(stakitt, rect)
    result
  }

  private def doColumn(xIndex: Int): Seq[StakittResult] = { // TODO
    yImageBorders.yPointListLo_pix.indices.dropRight(1).map(yIndex => constructStakittResult(xIndex, yIndex))
  }

  def measureLeafPositions(): Seq[StakittResult] = {

    // horzLine(yImageBorders.yPointListLo_pix, "Lo", 100, Color.black)
    // horzLine(yImageBorders.yPointListHi_pix, "Hi", 150, Color.white)

    val j = (0 until xAoiPairList.size).map(doColumn)
    //
    Trace.showInMSPaint(bufImg) // TODO rm

    Seq()

  }

}
