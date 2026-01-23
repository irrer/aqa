package org.aqa.webrun.winLutz360

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage
import org.aqa.webrun.wl.WLAnnotate
import org.aqa.webrun.wl.WLImageUtil

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import javax.vecmath.Point2d

object CompositeImage {

  private def d2i(d: Double): Int = d.round.toInt

  private def makeBoundingRectangle(analysis: Analysis): Rectangle = {

    /** extra space between the image's content and the image's edge. */
    val border_pix: Int = {
      val border_mm = 5.0
      val pix = analysis.trans.iso2PixDistX(border_mm).round.toInt
      pix
    }

    val edgeSet = analysis.edge.edgeSet
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
    * @param analysis Results of analysis.
    * @return Zoomed AOI.
    */
  private def makeInitialBufImage(preprocessedImage: DicomImage, analysis: Analysis, scale: Int): BufferedImage = {
    // convert to buffered image, using the central part of the ball as the brightest pixels.
    val img1 = BlankImage.make(preprocessedImage, analysis.edge.edgeSet)

    try {

      // restrict the image to the AOI
      val img2 = ImageUtil.subImage(img1, makeBoundingRectangle(analysis))

      // make AOI bigger.
      val img3 = ImageUtil.magnify(img2, scale)
      img3
    } catch {
      case _: Throwable => img1
    }

  }

  /**
    * Draw a line indicating where each edge is, and also crossing lines that shows where the center of the edges is.
    * @param bufImg Write on this image.
    * @param analysis The data.
    */
  private def drawEdgeLines(bufImg: BufferedImage, analysis: Analysis, scale: Int): Unit = {

    val edgeSet = analysis.edge.edgeSet
    val rect = makeBoundingRectangle(analysis)
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
    * @param analysis Data
    * @param scale Magnify the image by this factor.
    */
  private def drawBallLines(bufImg: BufferedImage, analysis: Analysis, scale: Int): Unit = {

    val outerRadius_pix: Int = analysis.trans.iso2PixDistX(analysis.machineWL.ballDiameter_mm).round.toInt
    val innerRadius_pix = outerRadius_pix / 2

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.yellow)
    val rect = makeBoundingRectangle(analysis)
    val si = ScaledImage(scale, rect.x, rect.y)

    val angle = analysis.collimator_deg

    if (analysis.ball.center_pix.isDefined) {
      val ballCenter_pix = analysis.ball.center_pix.get

      {
        val x1 = ballCenter_pix.x - outerRadius_pix
        val y1 = ballCenter_pix.y
        val x2 = ballCenter_pix.x + outerRadius_pix
        val y2 = ballCenter_pix.y

        val point1 = WLRotator.rotatePoint(new Point2d(x1, y1), ballCenter_pix, angle)
        val point2 = WLRotator.rotatePoint(new Point2d(x2, y2), ballCenter_pix, angle)

        si.drawLine(gc, point1, point2)
      }

      {
        val x1 = ballCenter_pix.x
        val y1 = ballCenter_pix.y - outerRadius_pix
        val x2 = ballCenter_pix.x
        val y2 = ballCenter_pix.y + outerRadius_pix

        val point1 = WLRotator.rotatePoint(new Point2d(x1, y1), ballCenter_pix, angle)
        val point2 = WLRotator.rotatePoint(new Point2d(x2, y2), ballCenter_pix, angle)

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
      si.drawLine(gc, ballCenter_pix, analysis.edge.edgeSet.center_pix)
    }
  }

  /**
    * Label each edge as X1, X2, Y1, Y2
    * @param bufImg Put text here.
    * @param analysis contains edges
    * @param scale scale of image
    */
  private def labelEdges(bufImg: BufferedImage, analysis: Analysis, scale: Int): Unit = {
    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.green)
    val rect = makeBoundingRectangle(analysis)
    val si = ScaledImage(scale, rect.x, rect.y)
    ImageText.setFont(gc, ImageText.DefaultFont, 30)

    def annotateOneEdge(edge: Edge): Unit = {
      val center = {
        val x = (edge.edgeLine.centerPoint.x + edge.loLine.centerPoint.x) / 2
        val y = (edge.edgeLine.centerPoint.y + edge.loLine.centerPoint.y) / 2
        new Point2d(x, y)
      }
      si.drawTextCenteredAt(gc, center.x, center.y, edge.name)
    }

    analysis.edge.edgeSet.edgeList.foreach(annotateOneEdge)
  }

  /**
    * Make a closeup image showing the centers of the edges and the ball.
    * @param analysis Data driving image.
    * @return An annotated image.
    */
  def makeCompositeImage(analysis: Analysis): BufferedImage = {

    val scale = WLImageUtil.calculateCloseupScale(analysis.al)

    val bufImg = makeInitialBufImage(analysis.preprocessedImage, analysis, scale)

    drawEdgeLines(bufImg, analysis, scale)

    drawBallLines(bufImg, analysis, scale)

    val annotate = new WLAnnotate(scale, 5)

    labelEdges(bufImg, analysis, scale)

    annotate.annotateImage( //
      png = bufImg,
      graphics = ImageUtil.getGraphics(bufImg),
      errorScaledX = analysis.offsetX_mm,
      errorScaledY = analysis.offsetY_mm,
      errorScaledXYCombined = analysis.offsetXY_mm,
      background = true,
      imageName = analysis.imageName,
      passLimit_mm = analysis.machineWL.passLimit_mm
    )

    bufImg
  }
}
