package org.aqa.webrun.wl.nonCardinal

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageDisplay
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage
import org.aqa.Config

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import javax.vecmath.Point2d

object WLNonCardCompositeImage {

  private def d2i(d: Double): Int = d.round.toInt

  private def scale: Int = {
    6
    Config.WLScale
  }

  private def makeBoundingRectangle(nonCard: WLNonCardAnalysis): Rectangle = {

    /** extra space between the image's content and the image's edge. */
    val border_pix: Int = {
      val border_mm = 5.0
      val pix = nonCard.trans.iso2PixDistX(border_mm).round.toInt
      pix
    }

    val edgeSet = nonCard.nonCardEdge.edgeSet
    val xList = edgeSet.intersectList.map(_.x)
    val yList = edgeSet.intersectList.map(_.y)
    val x = xList.min - border_pix
    val y = yList.min - border_pix
    val width = (xList.max - x) + (border_pix * 2)
    val height = (yList.max - y) + (border_pix * 2)

    val rectangle = new Rectangle(d2i(x), d2i(y), d2i(width), d2i(height))

    rectangle
  }

  /**
   * Make a zoomed image containing only the area of interest.
   * @param nonCard Ressults of analysis.
   * @return Zoomed AOI.
   */
  private def makeInitialBufImage(nonCard: WLNonCardAnalysis): BufferedImage = {

    // convert to image
    val dicomImage = new DicomImage(nonCard.al)

    // convert to buffered image, using the central part of the ball as the brightest pixels.
    val img1 = dicomImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), nonCard.minPixelValue, nonCard.maxPixelValue)

    // restrict the image to the AOI
    val img2 = ImageUtil.subImage(img1, makeBoundingRectangle(nonCard))

    // make AOI bigger.
    val img3 = ImageUtil.magnify(img2, scale)
    img3
  }

  /**
   * Draw a line indicating where each edge is, and also crossing lines that shows where the center of the edges is.
   * @param bufImg Write on this image.
   * @param nonCard The data.
   */
  private def drawEdgeLines(bufImg: BufferedImage, nonCard: WLNonCardAnalysis): Unit = {

    val edgeSet = nonCard.nonCardEdge.edgeSet
    val rect = makeBoundingRectangle(nonCard)
    val si = ScaledImage(scale, rect.x, rect.y)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    si.drawLine(gc, edgeSet.x1y1, edgeSet.x1y2)
    si.drawLine(gc, edgeSet.x1y1, edgeSet.x2y1)
    si.drawLine(gc, edgeSet.x1y1, edgeSet.x2y2)
    si.drawLine(gc, edgeSet.x1y2, edgeSet.x2y1)
    si.drawLine(gc, edgeSet.x1y2, edgeSet.x2y2)
    si.drawLine(gc, edgeSet.x2y1, edgeSet.x2y2)
  }

  private def drawBallLines(bufImg: BufferedImage, nonCard: WLNonCardAnalysis): Unit = {
    val innerRadius_mm = 2.5
    val outerRadius_mm = innerRadius_mm * 2
    val innerRadius_pix = nonCard.trans.iso2PixDistX(innerRadius_mm)
    val outerRadius_pix = nonCard.trans.iso2PixDistX(outerRadius_mm)

    val ballCenter_pix = nonCard.nonCardBall.center_pix

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)
    val rect = makeBoundingRectangle(nonCard)
    val si = ScaledImage(scale, rect.x, rect.y)

    si.drawLine(gc, new Point2d(ballCenter_pix.x - outerRadius_pix, ballCenter_pix.y), new Point2d(ballCenter_pix.x + outerRadius_pix, ballCenter_pix.y))
    si.drawLine(gc, new Point2d(ballCenter_pix.x, ballCenter_pix.y - outerRadius_pix), new Point2d(ballCenter_pix.x, ballCenter_pix.y + outerRadius_pix))

    def makeCircle(radius: Double): Unit = {
      val x = si.scalePixelX(ballCenter_pix.x - radius)
      val width = si.scalePixelX(radius * 2)

      val y = si.scalePixelY(ballCenter_pix.y - radius)
      val height = si.scalePixelY(radius * 2)
      gc.drawOval(x, y, width, height)
    }

    makeCircle(innerRadius_pix)
    gc.setColor(Color.yellow)
    makeCircle(outerRadius_pix)
  }

  def makeCompositeImage(nonCard: WLNonCardAnalysis): BufferedImage = {

    val bufImg = makeInitialBufImage(nonCard)

    drawEdgeLines(bufImg, nonCard)

    drawBallLines(bufImg, nonCard)

    if (false) { // TODO rm
      ImageDisplay.showInMSPaint(bufImg)
    }

    bufImg
  }
}
