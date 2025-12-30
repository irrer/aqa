package org.aqa.webrun.wl.nonCardinal

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage
import org.aqa.webrun.wl.WLImageUtil
import org.aqa.webrun.wl.WLPreprocessImage

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import javax.vecmath.Point2d

object WLNonCardCompositeImage {

  private def d2i(d: Double): Int = d.round.toInt

  /*
  private def scale: Int = {
    Config.WLScale
    7
  }
   */

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
    val width = (xList.max - xList.min) + (border_pix * 2)
    val height = (yList.max - yList.min) + (border_pix * 2)

    val rectangle = new Rectangle(d2i(x), d2i(y), d2i(width), d2i(height))

    rectangle
  }

  /**
    * Make a zoomed image containing only the area of interest.
    * @param nonCard Results of analysis.
    * @return Zoomed AOI.
    */
  private def makeInitialBufImage(nonCard: WLNonCardAnalysis, scale: Int): BufferedImage = {

    // convert to image
    val preprocessedImage = WLPreprocessImage(nonCard.al, None).preprocessedImage //  new DicomImage(nonCard.al)

    val maxPix = {
      val centerX = nonCard.nonCardBall.center_pix.getX.round.toInt
      val centerY = nonCard.nonCardBall.center_pix.getY.round.toInt
      val range = -2 until 3
      val list = for (x <- range; y <- range) yield preprocessedImage.get(x + centerX, y + centerY)
      list.sum / list.size
    }
    val minPix = preprocessedImage.pixelData.flatten.sorted.slice(5, 15).sum / 10

    // convert to buffered image, using the central part of the ball as the brightest pixels.
    val img1 = preprocessedImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), minPix, maxPix)

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
  private def drawEdgeLines(bufImg: BufferedImage, nonCard: WLNonCardAnalysis, scale: Int): Unit = {

    val edgeSet = nonCard.nonCardEdge.edgeSet
    val rect = makeBoundingRectangle(nonCard)
    val si = ScaledImage(scale, rect.x, rect.y)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.green)

    si.drawLine(gc, edgeSet.x1y1, edgeSet.x1y2)
    si.drawLine(gc, edgeSet.x1y1, edgeSet.x2y1)
    si.drawLine(gc, edgeSet.x1y1, edgeSet.x2y2)
    si.drawLine(gc, edgeSet.x1y2, edgeSet.x2y1)
    si.drawLine(gc, edgeSet.x1y2, edgeSet.x2y2)
    si.drawLine(gc, edgeSet.x2y1, edgeSet.x2y2)
  }

  /**
    * Draw circles centered around the center of the ball, and lines that intersect at the center.  Rotate
    * the lines by the collimator angle so that they don't coincide with the edge lines.
    * @param bufImg Draw on this image
    * @param nonCard Data
    * @param scale Magnify the image by this factor.
    */
  private def drawBallLines(bufImg: BufferedImage, nonCard: WLNonCardAnalysis, scale: Int): Unit = {

    val outerRadius_pix: Int = nonCard.trans.iso2PixDistX(nonCard.machineWL.ballDiameter_mm).round.toInt
    val innerRadius_pix = outerRadius_pix / 2

    val ballCenter_pix = nonCard.nonCardBall.center_pix

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.yellow)
    val rect = makeBoundingRectangle(nonCard)
    val si = ScaledImage(scale, rect.x, rect.y)

    val angle = nonCard.collimator_deg
    val center = nonCard.nonCardBall.center_pix

    {
      val x1 = ballCenter_pix.x - outerRadius_pix
      val y1 = ballCenter_pix.y
      val x2 = ballCenter_pix.x + outerRadius_pix
      val y2 = ballCenter_pix.y

      val point1 = WLRotator.rotatePoint(new Point2d(x1, y1), center, angle)
      val point2 = WLRotator.rotatePoint(new Point2d(x2, y2), center, angle)

      si.drawLine(gc, point1, point2)
    }

    {
      val x1 = ballCenter_pix.x
      val y1 = ballCenter_pix.y - outerRadius_pix
      val x2 = ballCenter_pix.x
      val y2 = ballCenter_pix.y + outerRadius_pix

      val point1 = WLRotator.rotatePoint(new Point2d(x1, y1), center, angle)
      val point2 = WLRotator.rotatePoint(new Point2d(x2, y2), center, angle)

      si.drawLine(gc, point1, point2)
    }

    def makeCircle(radius: Double): Unit = {
      val x = ballCenter_pix.x - radius
      val width = radius * 2

      val y = ballCenter_pix.y - radius
      val height = radius * 2
      si.drawOval(gc, x, y, width, height)
    }

    makeCircle(innerRadius_pix)
    makeCircle(outerRadius_pix)

    gc.setColor(Color.red)
    ImageUtil.setLineThickness(gc, 2.0)
    si.drawLine(gc, ballCenter_pix, nonCard.nonCardEdge.edgeSet.center_pix)
  }

  def makeCompositeImage(nonCard: WLNonCardAnalysis): BufferedImage = {

    val scale = WLImageUtil.calculateCloseupScale(nonCard.al)

    val bufImg = makeInitialBufImage(nonCard, scale)

    drawEdgeLines(bufImg, nonCard, scale)

    drawBallLines(bufImg, nonCard, scale)

    bufImg
  }
}
