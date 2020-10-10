package org.aqa.webrun.bbByEpid

import com.pixelmed.dicom.AttributeList
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.DicomImage
import java.awt.geom.Point2D
import java.awt.Color
import java.awt.Rectangle
import org.aqa.Config
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ImageText
import org.aqa.Util
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import javax.vecmath.Point2d
import java.awt.BasicStroke
import edu.umro.ScalaUtil.DicomUtil
import java.text.SimpleDateFormat
import java.awt.Polygon

/**
 * Create user friendly images and annotate them.
 */
class BBbyEPIDAnnotateImages(al: AttributeList, bbLoc_mmGantry: Option[Point2D.Double], description: Option[String]) {

  val bbLoc_mmIso = {
    if (bbLoc_mmGantry.isDefined) {
      Some(new Point2D.Double(bbLoc_mmGantry.get.getX, -bbLoc_mmGantry.get.getY))
    } else
      None
  }

  private val textPointSize = 30

  private def d2i(d: Double) = d.round.toInt

  // the circle drawn around the BB should be this many times its size
  private val circleRadiusScale = 2.0

  private val trans = new IsoImagePlaneTranslator(al)
  private def bbLoc_pix = trans.iso2Pix(bbLoc_mmIso.get) // location of BB in pixels
  private val fullImage = new DicomImage(al)
  private val radius_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) // radius of the BB in pixels

  val offsetText = {
    if (description.isDefined)
      description.get.trim
    else
      "Offset Not Available"
  }

  private def circleRadius_pix(scale: Int) = radius_pix * circleRadiusScale * scale

  /**
   *  draw a circle with an X in it centered around the BB
   *
   *  @offset Offset from original image
   *
   *  @bufImage Draw in this image
   *
   *  @scale Scale of bufImage compared to original
   */
  private def drawCircleWithXAtCenterOfBB(offset: Point2D.Double, bufImage: BufferedImage, scale: Int) = {
    if (bbLoc_mmIso.isDefined) {
      val graphics = ImageUtil.getGraphics(bufImage)
      graphics.setColor(Color.white)
      ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)

      val psX = ((bbLoc_pix.getX - offset.getX + 0.5) * scale)
      val psY = ((bbLoc_pix.getY - offset.getY + 0.5) * scale)
      val x = psX - circleRadius_pix(scale)
      val y = psY - circleRadius_pix(scale)
      val w = circleRadius_pix(scale) * 2
      val h = w

      val lineOffset = circleRadius_pix(scale) / Math.sqrt(2.0)

      ImageUtil.setLineThickness(graphics, 2.0)
      graphics.drawOval(d2i(x), d2i(y), d2i(w), d2i(h))
      ImageUtil.setLineThickness(graphics, 1.0)

      graphics.drawLine(
        d2i(psX - lineOffset), d2i(psY - lineOffset),
        d2i(psX + lineOffset), d2i(psY + lineOffset))

      graphics.drawLine(
        d2i(psX + lineOffset), d2i(psY - lineOffset),
        d2i(psX - lineOffset), d2i(psY + lineOffset))

      ImageText.drawTextCenteredAt(graphics, psX, y - circleRadius_pix(scale), "X is BB center")

      ImageText.drawTextCenteredAt(graphics, psX, y - circleRadius_pix(scale) + textPointSize, offsetText)
    }
  }

  /**
   * Draw a + at the center showing where the plan isocenter is.
   *
   * @param center Center of + in pixels, not scaled.
   */
  private def drawCirclesAtCenterOfPlan(center: Point2D.Double, bufImage: BufferedImage, scale: Int) = {
    val scaledCenter = new Point2d(center.getX, center.getY)
    scaledCenter.scale(scale)

    val graphics = ImageUtil.getGraphics(bufImage)
    graphics.setColor(Color.red)

    val radius = scale * 2
    val skip = 2
    val diam = radius * 2
    val xi = d2i(center.getX * scale)
    val yi = d2i(center.getY * scale)

    graphics.drawLine(xi - radius, yi, xi - skip, yi)
    graphics.drawLine(xi + skip, yi, xi + radius, yi)

    graphics.drawLine(xi, yi - radius, xi, yi - skip)
    graphics.drawLine(xi, yi + skip, xi, yi + radius)

    graphics.setStroke(new BasicStroke(3))
    graphics.drawOval(xi - radius, yi - radius, diam, diam)

    val thickness = d2i(scale * 0.25)
    val len = thickness * 3

    def top = {
      val poly = new Polygon
      poly.addPoint(xi, yi - radius)
      poly.addPoint(xi - thickness, yi - radius)
      poly.addPoint(xi, yi - radius + len)
      poly.addPoint(xi + 1, yi - radius + len)
      poly.addPoint(xi + thickness, yi - radius)
      Trace.trace(poly)
      graphics.fill(poly)
    }

    def bottom = {
      val poly = new Polygon
      poly.addPoint(xi, yi + radius)
      poly.addPoint(xi - thickness, yi + radius)
      poly.addPoint(xi, yi + radius - len)
      poly.addPoint(xi + 1, yi + radius - len)
      poly.addPoint(xi + thickness, yi + radius)
      Trace.trace(poly)
      graphics.fill(poly)
    }

    def left = {
      val poly = new Polygon
      poly.addPoint(xi - radius, yi)
      poly.addPoint(xi - radius, yi - thickness + 1)
      poly.addPoint(xi - radius + len, yi)
      poly.addPoint(xi - radius + len, yi + 1)
      poly.addPoint(xi - radius, yi + thickness)
      Trace.trace(poly)
      graphics.fill(poly)
    }

    def right = {
      val poly = new Polygon
      poly.addPoint(xi + radius, yi)
      poly.addPoint(xi + radius, yi - thickness + 1)
      poly.addPoint(xi + radius - len, yi)
      poly.addPoint(xi + radius - len, yi + 1)
      poly.addPoint(xi + radius, yi + thickness)
      Trace.trace(poly)
      graphics.fill(poly)
    }

    top
    bottom
    left
    right
  }

  private def makeFullBufImg: BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 4

    val fullSize = ImageUtil.magnify(fullImage.toDeepColorBufferedImage(0.05), scale)
    Config.applyWatermark(fullSize)

    drawCircleWithXAtCenterOfBB(new Point2D.Double(0, 0), fullSize, scale)
    drawCirclesAtCenterOfPlan(trans.caxCenter_pix, fullSize, scale)
    fullSize
  }

  private def putDateTime(bufImage: BufferedImage) = {
    val graphics = ImageUtil.getGraphics(bufImage)

    val dateOpt = DicomUtil.getTimeAndDate(al, TagFromName.ContentDate, TagFromName.ContentTime)
    if (dateOpt.isDefined) {
      val date = dateOpt.get
      val angle = Util.angleRoundedTo90(al.get(TagFromName.GantryAngle).getDoubleValues()(0))
      val dateFormat = new SimpleDateFormat("m/d/yyyy H:mm")
      val text = "G" + angle + " - " + dateFormat.format(date)
      val dim = ImageText.getTextDimensions(graphics, text)

      ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
      graphics.setColor(Color.black)
      graphics.fillRect(0, 0, dim.getWidth.toInt, dim.getHeight.toInt)
      graphics.setColor(Color.white)
      graphics.drawString(text, textPointSize, textPointSize)
    }
  }

  private def makeCloseupBufImg: BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 32

    // defines how much area around the bb should be used to make the close up image
    val closeupScale = 10.0
    val closeup_mm = Config.EPIDBBPenumbra_mm * closeupScale

    // position of upper left corner of close up image in pixel coordinates
    val upperLeftCorner = {
      if (bbLoc_mmIso.isDefined) {
        trans.iso2Pix(bbLoc_mmIso.get.getX - closeup_mm, bbLoc_mmIso.get.getY - closeup_mm)
      } else {
        // if no BB found, then just make a close up of the center of the image
        trans.iso2Pix(-closeup_mm, -closeup_mm)
      }
    }

    val center = {
      val pixCenter = trans.iso2Pix(-trans.caxCenter_iso.getX, trans.caxCenter_iso.getY) // TODO experimental
      new Point2D.Double((pixCenter.getX - upperLeftCorner.getX), (pixCenter.getY - upperLeftCorner.getY))
    }

    // define rectangle for copying just the middle of the image
    val closeupRect = {
      val x = d2i(upperLeftCorner.getX)
      val y = d2i(upperLeftCorner.getY)
      val w = d2i(trans.iso2PixDistX(closeup_mm * 2))
      val h = d2i(trans.iso2PixDistY(closeup_mm * 2))
      new Rectangle(x, y, w, h)
    }

    val closeupImage = ImageUtil.magnify(fullImage.getSubimage(closeupRect).toBufferedImage(Config.EPIDImageColor), scale)
    putDateTime(closeupImage)
    Config.applyWatermark(closeupImage)

    drawCircleWithXAtCenterOfBB(upperLeftCorner, closeupImage, scale)
    drawCirclesAtCenterOfPlan(center, closeupImage, scale)

    closeupImage
  }

  val fullBufImg = makeFullBufImg
  val closeupBufImg = makeCloseupBufImg

}