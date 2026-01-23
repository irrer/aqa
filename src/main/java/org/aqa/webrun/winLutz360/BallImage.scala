package org.aqa.webrun.winLutz360

import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import javax.vecmath.Point2d
import javax.vecmath.Point2i

/**
  * Utilities for making user-friendly ball images.
  *
  * @param analysis Results of WL analysis.
  */
case class BallImage(analysis: Analysis) {

  /**
    * Make a list of whole pixels that are within the ball AOI.
    */
  private val ballPointList: Seq[Point2i] = {

    val pointList =
      for ( //
        x <- 0 until analysis.preprocessedImage.width;
        y <- 0 until analysis.preprocessedImage.height;
        if analysis.edge.edgeSet.pointIsInBallAoi(new Point2d(x, y))
      )
        yield new Point2i(x, y)

    pointList

  }

  private val ballRectangle: Option[Rectangle] = {
    if (ballPointList.isEmpty)
      None
    else {
      val minX = ballPointList.map(_.x).min
      val maxX = ballPointList.map(_.x).max
      val minY = ballPointList.map(_.y).min
      val maxY = ballPointList.map(_.y).max

      val rectangle = new Rectangle(minX, minY, maxX - minX, maxY - minY)

      Some(rectangle)
    }
  }

  private val ballScale = 20

  /**
    * Add annotation to a scaled ball image.
    * @param ballImage Annotate this.
    */
  private def annotateScaledBallImage(ballImage: BufferedImage): Unit = {
    if (analysis.ball.center_pix.isDefined && ballRectangle.isDefined && ballPointList.nonEmpty) {
      val si = ScaledImage(ballScale, ballRectangle.get.x, ballRectangle.get.y)

      val center_pix = analysis.ball.center_pix.get

      val lineLength_pix = 5

      val gc = ImageUtil.getGraphics(ballImage)
      gc.setColor(Color.yellow)

      si.drawLine(gc, center_pix.x - lineLength_pix, center_pix.y, center_pix.x + lineLength_pix, center_pix.y)

      si.drawLine(gc, center_pix.x, center_pix.y - lineLength_pix, center_pix.x, center_pix.y + lineLength_pix)
    }
  }

  /**
    * Make an image of just the ball with the center marked.
    * @return Image of ball.
    */
  def ballImage(): Option[BufferedImage] = {

    if (ballPointList.isEmpty || ballRectangle.isEmpty)
      None
    else {
      val bufImg: BufferedImage = {
        val fullImage = BlankImage.make(analysis.preprocessedImage, analysis.edge.edgeSet)
        val subImage = ImageUtil.subImage(fullImage, ballRectangle.get)
        val scaledImage = ImageUtil.magnify(subImage, ballScale)
        scaledImage
      }

      annotateScaledBallImage(bufImg)

      Some(bufImg)
    }
  }

  /**
    * Make a zoomed and annotated image of the ball that highlights variations in the background surrounding the
    * ball.  This image illustrates the effects of the stem that supports the ball.
    * @return Zoomed, annotated, ball image.
    */
  def ballBackgroundImage(): Option[BufferedImage] = {
    if (ballPointList.isEmpty || ballRectangle.isEmpty)
      None
    else {
      val subImg = analysis.preprocessedImage.getSubimage(ballRectangle.get)

      val stdDev = ImageUtil.stdDev(subImg.pixelData.flatten)

      val limit = stdDev * .5

      val min = subImg.pixelData.flatten.sorted.slice(5, 15).sum / 10
      val backgroundImage = subImg.fun1(p => if (p > limit) min else p)

      val bufImg = {
        val img = backgroundImage.toBufferedImage(Color.blue)
        ImageUtil.magnify(img, ballScale)
      }

      annotateScaledBallImage(bufImg)

      Some(bufImg)
    }
  }

}
