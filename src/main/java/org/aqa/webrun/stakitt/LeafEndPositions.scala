package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries

import java.awt.geom.Rectangle2D
import java.awt.Color
import scala.util.Random

case class LeafEndPositions(dicomImage: DicomImage, xImageBorders: LeafEnds, yImageBorders: LeafBoundaries, rtimage: AttributeList) extends Logging {

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
      val lo = mean(xL0, xL1)
      val hi = xL0 + (xFilled / 2)
      XAoi(lo, hi)
    }

    val mid = xList.indices.drop(1).dropRight(1).map(toXAoi)

    val last = {
      val lo = ((xL1 - xL2) / 2) + xL0
      val hi = x0 - ((xList(2) - x1) / 2)
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
  val bufImg = {
    val img = dicomImage.toDeepColorBufferedImage(0.01)
    ImageUtil.magnify(img, scale)
  }
  val gc = ImageUtil.getGraphics(bufImg)
  gc.setColor(Color.white)

  val rand = new Random()
  val colorList = Seq(Color.white, Color.pink, Color.lightGray, Color.yellow)

  val si = ScaledImage(scale, 0, 0)

  def vertLine(x: Double): Unit = si.drawLine(gc, x, 0, x, dicomImage.height)

  def horzLine(lineList: Seq[Double], name: String, offset: Int, color: Color): Unit = {
    gc.setColor(color)
    val rect = ImageText.getTextDimensions(gc, name)
    ImageText.drawTextCenteredAt(gc, (rect.getWidth / 2) + 20, (rect.getHeight + 1) * 2 * offset, name)
    lineList.foreach(y => si.drawLine(gc, 0, y, dicomImage.width, y))
  }

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

  private def aoi(xIndex: Int, yIndex: Int): Rect2d = {

    val xAoi = xAoiPairList(xIndex)

    val yDelta = yRange * (xAoi.mid / xRange)

    val rect = new Rect2d(xAoi.lo, yImageBorders.yPointListLo_pix(yIndex) + yDelta, xAoi.width, yHeight)
    rect
  }

  private def measureLeafEnd(xIndex: Int, yIndex: Int): Int = { // StakittResult = { // TODO change type

    val rect = aoi(xIndex, yIndex)
    drawRect(rect)
    0
  }

  private def doColumn(xIndex: Int): Seq[StakittResult] = { // TODO
    yImageBorders.yPointListLo_pix.indices.drop(1).map(yIndex => measureLeafEnd(xIndex, yIndex))
    Seq()
  }

  def measureLeafPositions(): Seq[StakittResult] = {

    (0 until xAoiPairList.size).map(doColumn)
    //
    Trace.showInMSPaint(bufImg) // TODO rm

    Seq()

  }

}
