package org.aqa.webrun.wl

import org.aqa.Config
import org.aqa.webrun.wl.WLProcessImage._

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.RenderingHints

class WLAnnotate(SCALE: Int, BALL_RADIUS: Int) {

  /**
    * Translate a source image coordinate into the coordinate system used to draw graphics.
    * Add the 0.5 to get to the center of the displayed pixel.
    */
  private def coordinate(x: Double): Int = {
    ((x + 0.5) * SCALE).toInt
  }

  /**
    * draw line between center of square and center of ball
    */
  def drawBoxBallOffset(graphics: Graphics2D, ballCenter: (Double, Double), boxCenter: (Double, Double)): Unit = {
    graphics.setColor(Config.WLOffsetColor)

    graphics.drawLine(coordinate(ballCenter._1), coordinate(ballCenter._2), coordinate(boxCenter._1), coordinate(boxCenter._2))
    graphics.drawLine(coordinate(ballCenter._1) - 1, coordinate(ballCenter._2), coordinate(boxCenter._1) - 1, coordinate(boxCenter._2))
    graphics.drawLine(coordinate(ballCenter._1) + 1, coordinate(ballCenter._2), coordinate(boxCenter._1) + 1, coordinate(boxCenter._2))
    graphics.drawLine(coordinate(ballCenter._1), coordinate(ballCenter._2) - 1, coordinate(boxCenter._1), coordinate(boxCenter._2) - 1)
    graphics.drawLine(coordinate(ballCenter._1), coordinate(ballCenter._2) + 1, coordinate(boxCenter._1), coordinate(boxCenter._2) + 1)
  }

  /**
    * Draw the horizontal and vertical lines marking the center of something.  By convention in
    * this project, it marks the center of the ball.
    */
  def drawCross(graphics: Graphics2D, x: Double, y: Double, markerSize: Double): Unit = {
    graphics.drawLine(coordinate(x - markerSize), coordinate(y), coordinate(x + markerSize), coordinate(y))
    graphics.drawLine(coordinate(x), coordinate(y - markerSize), coordinate(x), coordinate(y + markerSize))

  }

  private def drawCircles(graphics: Graphics2D, x: Double, y: Double): Unit = {
    graphics.setColor(Color.yellow)
    val NUM_CIRCLE = Config.WLNumberOfCircles

    def graphicDistance(d: Double): Int = (d * SCALE + .5).toInt

    for (circle <- 1 to NUM_CIRCLE) {
      val radius = (circle.toDouble / NUM_CIRCLE) * BALL_RADIUS
      graphics.drawOval(coordinate(x - radius), coordinate(y - radius), graphicDistance(radius * 2), graphicDistance(radius * 2))
    }
  }

  def highlightWLBadPixelList(badPixelList: Seq[WLBadPixel], graphics: Graphics2D): Unit = {
    val circleRadius = Config.WLBadPixelCorrectionRadius
    graphics.setColor(Config.WLFailColor)

    def highlightWLBadPixel(badPixel: WLBadPixel): Unit = {
      val hp = SCALE / 2

      def crd(x: Double): Int = ((x * SCALE) + 0.5).toInt

      val x = badPixel.x
      val y = badPixel.y

      (0 until 9).foreach(i => {
        val rx = circleRadius + ((i % 3).toFloat / SCALE)
        val ry = circleRadius + ((i / 3).toFloat / SCALE)
        graphics.drawOval(
          crd((x - rx) + hp), // x
          crd((y - ry) + hp), // y
          crd(rx * 2), // width
          crd(ry * 2) // height
        )
      })
      (0 until 3).foreach(i =>
        graphics.drawRect(
          crd(x) - i, // x
          crd(y) - i, // y
          crd(SCALE + (2 * i)), // width
          crd(SCALE + (2 * i)) // height
        )
      )
    }

    badPixelList.foreach(b => highlightWLBadPixel(b))
  }

  def saveFineLocatedImage(aoi: IndexedSeq[IndexedSeq[Float]], xPosition: SearchRange, yPosition: SearchRange): BufferedImage = {
    val rSpline = toCubicSpline(unitize(rowSum(aoi)).map(x => (x * .5).toFloat))
    val cSpline = toCubicSpline(unitize(colSum(aoi)).map(x => (x * .5).toFloat))

    val width = aoi(0).length
    val height = aoi.length
    val png = toPngScaled(aoi, SCALE)

    for (x <- -SCALE until width * SCALE) {
      val y = (height * SCALE - 1 - (cSpline.evaluate(x.toDouble / SCALE) * (height * SCALE - 2))).toInt
      val xo = x + (SCALE / 2)
      if ((xo >= 0) && (xo < (width * SCALE))) png.setRGB(xo, boundInt(y, 0, height * SCALE - 1), Config.WLSplineColor.getRGB)
    }

    for (y <- -SCALE until height * SCALE) {
      val x = (1 + (rSpline.evaluate(y.toDouble / SCALE) * (width * SCALE - 2))).toInt
      val yo = y + (SCALE / 2)
      if ((yo >= 0) && (yo < (height * SCALE))) png.setRGB(boundInt(x, 0, width * SCALE - 1), yo, Config.WLSplineColor.getRGB)
    }

    val graphics = png.getGraphics.asInstanceOf[Graphics2D]
    graphics.setColor(Color.yellow)

    drawCross(graphics, xPosition.center, yPosition.center, 20)

    drawCircles(graphics, xPosition.center, yPosition.center)

    graphics.setColor(Config.WLSplineColor)
    graphics.drawLine(coordinate(xPosition.lo), 0, coordinate(xPosition.lo), coordinate(height))
    graphics.drawLine(coordinate(xPosition.hi), 0, coordinate(xPosition.hi), coordinate(height))

    graphics.setColor(Config.WLSplineColor)
    graphics.drawLine(0, coordinate(yPosition.lo), coordinate(width), coordinate(yPosition.lo))
    graphics.drawLine(0, coordinate(yPosition.hi), coordinate(width), coordinate(yPosition.hi))

    png
  }

  /**
    * Draw graphics to indicate where the ball is.
    * Draw outline of ball and diagonal cross-hairs at center of ball
    */
  def drawBallGraphics(graphics: Graphics2D, ballCenterX: Double, ballCenterY: Double): Unit = {
    graphics.setColor(Color.yellow)

    drawCircles(graphics, ballCenterX, ballCenterY)
    drawCross(graphics, ballCenterX, ballCenterY, BALL_RADIUS)
  }

  /**
    * Draw the lines that show where the box has been located.
    */
  def drawBoxGraphics(graphics: Graphics2D, top: Double, bottom: Double, left: Double, right: Double): Unit = {
    graphics.setColor(Color.green)
    graphics.drawLine(coordinate(left), coordinate(top), coordinate(left), coordinate(bottom)) // vertical line left
    graphics.drawLine(coordinate(right), coordinate(top), coordinate(right), coordinate(bottom)) // vertical line right
    graphics.drawLine(coordinate(left), coordinate(top), coordinate(right), coordinate(top)) // horizontal line top
    graphics.drawLine(coordinate(left), coordinate(bottom), coordinate(right), coordinate(bottom)) // horizontal line bottom

    graphics.drawLine(coordinate(left), coordinate(top), coordinate(right), coordinate(bottom)) // diagonal left-top to right-bottom
    graphics.drawLine(coordinate(right), coordinate(top), coordinate(left), coordinate(bottom)) // diagonal right-top to left-bottom
  }

  /**
    * Annotate the image with words and numbers.
    */
  def annotateImage(
      png: BufferedImage,
      graphics: Graphics2D,
      errorScaledX: Double,
      errorScaledY: Double,
      errorScaledXYCombined: Double,
      background: Boolean,
      imageName: String,
      passLimit_mm: Double
  ): WLImageStatus.Value = {
    def fmt(d: Double): String = "%6.2f".format(d).trim

    graphics.setColor(Config.WLTextColor)

    val font = GraphicFont.getFont
    graphics.setFont(font)
    graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)

    val spacer = "        "
    val text1 = "Offset in mm:    X = " + fmt(errorScaledX) + spacer + " Y = " + fmt(errorScaledY) + spacer
    val frc = GraphicFont.getFontRenderContext
    val stringRectangle1 = font.getStringBounds(text1, frc)
    val xPosition1 = (png.getWidth - stringRectangle1.getWidth) / 2
    val yPosition1 = stringRectangle1.getHeight
    graphics.drawString(text1, xPosition1.toInt, yPosition1.toInt)

    val combinedXY = "R = " + fmt(errorScaledXYCombined)
    val passed = if (errorScaledXYCombined <= passLimit_mm) WLImageStatus.Passed else WLImageStatus.OffsetLimitExceeded
    val statusText: String = {
      if (passed == WLImageStatus.Passed) "PASSED" else "FAILED"
    }

    val statusColor = if (passed == WLImageStatus.Passed) Config.WLPassColor else Config.WLFailColor
    graphics.setBackground(statusColor)
    val text2 = combinedXY + spacer + statusText
    val stringRectangle2 = font.getStringBounds(text2, frc)
    val xPosition2 = (png.getWidth - font.getStringBounds(text2, frc).getWidth) / 2
    val yPosition2 = stringRectangle1.getHeight + stringRectangle2.getHeight
    val stringRectangleStatus = font.getStringBounds(statusText, frc)

    val statusWidth = stringRectangleStatus.getWidth.toInt
    val statusHeight = graphics.getFontMetrics.getMaxAscent
    val statusX = (xPosition2 + stringRectangle2.getWidth - stringRectangleStatus.getWidth).toInt
    val statusY = yPosition2 - stringRectangleStatus.getHeight + ((graphics.getFontMetrics.getHeight - graphics.getFontMetrics.getAscent) * 1.5 - 1)
    if (background) graphics.clearRect(statusX-5, statusY.toInt, statusWidth + 10, statusHeight)
    graphics.drawString(text2, xPosition2.toInt, yPosition2.toInt)

    val stringRectangle3 = font.getStringBounds(imageName, frc)
    val xPosition3 = (png.getWidth - stringRectangle3.getWidth) / 2
    val yPosition3 = png.getHeight - stringRectangle3.getHeight
    graphics.drawString(imageName, xPosition3.toInt, yPosition3.toInt)

    passed
  }

}
