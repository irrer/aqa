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
import javax.vecmath.Point2i
import org.aqa.OrientationWatermark

object BBbyEPIDAnnotateImages {
  // get the date and time via either the Content or AcquisitionF
  def epidDateText(attrList: AttributeList): Option[String] = {
    val dateTimeList = Seq(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime))
    val dateTime = dateTimeList.map(dt => DicomUtil.getTimeAndDate(attrList, dt._1, dt._2)).flatten.headOption
    val dateFormat = new SimpleDateFormat("M/d/yyyy H:mm:ss")
    if (dateTime.isDefined) Some(dateFormat.format(dateTime.get)) else None
  }
}

/**
 * Create user friendly images and annotate them.
 */
class BBbyEPIDAnnotateImages(al: AttributeList, bbLoc_mmGantry: Option[Point2D.Double], description: Option[String], bbCenter_pix: Option[Point2d]) {

  private val trans = new IsoImagePlaneTranslator(al)
  val gantryAngleRounded = Util.angleRoundedTo90(Util.gantryAngle(al))

  private def d2D(d: Point2d) = new Point2D.Double(d.getX, d.getY)

  private val bbLoc_mmIso = {
    if (bbCenter_pix.isDefined) Some(trans.pix2Iso(d2D(bbCenter_pix.get)))
    else None
  }

  private val planCenter_pix = trans.caxCenter_pix
  //Trace.trace(ga + "   bbCenter_pix: " + bbCenter_pix + "    caxCenter_pix: " + planCenter_pix + "    diff: " + (bbCenter_pix.get.getX - planCenter_pix.getX) + ", " + (bbCenter_pix.get.getY - planCenter_pix.getY))

  private val textPointSize = 30

  private def d2i(d: Double) = d.round.toInt

  // The circle drawn around the BB should be this many times the BB's size
  private val circleRadiusScale = 2.0

  private val fullImage = new DicomImage(al)
  private val radius_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) // radius of the BB in pixels

  val offsetText = {
    if (description.isDefined)
      description.get.trim
    else
      "Offset Not Available"
  }

  private def circleRadius_pix(scale: Int) = radius_pix * circleRadiusScale * scale

  private def applyOrientationWatermark(image: BufferedImage) = {
    gantryAngleRounded match {
      case 0 => OrientationWatermark.mark(OrientationWatermark.Frontal, image)
      case 90 => OrientationWatermark.mark(OrientationWatermark.SagittalLeft, image)
      case 180 => OrientationWatermark.mark(OrientationWatermark.Posterior, image)
      case 270 => OrientationWatermark.mark(OrientationWatermark.SagittalRight, image)
      case _ =>
    }
  }

  /**
   *  draw a circle with an X in it centered around the BB
   *
   *  @offset Offset from original image
   *
   *  @bufImage Draw in this image
   *
   *  @scale Scale of bufImage compared to original
   */
  private def drawCircleWithXAtCenterOfBB(offset: Point2d, bufImage: BufferedImage, scale: Int) = {
    if (bbCenter_pix.isDefined) {
      val graphics = ImageUtil.getGraphics(bufImage)
      graphics.setColor(Color.white)
      ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)

      val psX = Util.scalePixel(scale, bbCenter_pix.get.getX - offset.getX)
      val psY = Util.scalePixel(scale, bbCenter_pix.get.getY - offset.getY)
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

      ImageText.drawTextCenteredAt(graphics, psX, y - circleRadius_pix(scale) - 2 * textPointSize, "X is BB center")

      ImageText.drawTextCenteredAt(graphics, psX, y - circleRadius_pix(scale) - textPointSize, offsetText)
    }
  }

  /**
   * Draw a + at the center showing where the plan isocenter is.
   *
   * @param center Center of + in pixels, not scaled.
   */
  private def drawCirclesAtCenterOfPlan(offset: Point2d, bufImage: BufferedImage, scale: Int) = {

    val planCenterOffsetAndScaled_pix = new Point2D.Double(
      Util.scalePixel(scale, planCenter_pix.getX - offset.getX),
      Util.scalePixel(scale, planCenter_pix.getY - offset.getY))

    val graphics = ImageUtil.getGraphics(bufImage)
    graphics.setColor(Color.red)

    val radius = scale * 2
    val skip = 2
    val diam = radius * 2
    val xi = d2i(planCenterOffsetAndScaled_pix.getX)
    val yi = d2i(planCenterOffsetAndScaled_pix.getY)

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
      graphics.fill(poly)
    }

    def bottom = {
      val poly = new Polygon
      poly.addPoint(xi, yi + radius)
      poly.addPoint(xi - thickness, yi + radius)
      poly.addPoint(xi, yi + radius - len)
      poly.addPoint(xi + 1, yi + radius - len)
      poly.addPoint(xi + thickness, yi + radius)
      graphics.fill(poly)
    }

    def left = {
      val poly = new Polygon
      poly.addPoint(xi - radius, yi)
      poly.addPoint(xi - radius, yi - thickness + 1)
      poly.addPoint(xi - radius + len, yi)
      poly.addPoint(xi - radius + len, yi + 1)
      poly.addPoint(xi - radius, yi + thickness)
      graphics.fill(poly)
    }

    def right = {
      val poly = new Polygon
      poly.addPoint(xi + radius, yi)
      poly.addPoint(xi + radius, yi - thickness + 1)
      poly.addPoint(xi + radius - len, yi)
      poly.addPoint(xi + radius - len, yi + 1)
      poly.addPoint(xi + radius, yi + thickness)
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

    val fullSize =
      if (false)
        ImageUtil.magnify(fullImage.toDeepColorBufferedImage(0.05), scale) // makes color image
      else {
        // use the pixels near the BB to determine the windowing and leveling of the greyscale image.
        val dist = 15
        // If the BB is defined, then use the area around it.  if it is not defined, the use the middle of the image.
        val center = {
          val c = if (bbCenter_pix.isDefined) d2D(bbCenter_pix.get) else trans.iso2Pix(0, 0)
          new Point2i(c.getX.round.toInt, c.getY.round.toInt)
        }
        val pixList = for (
          x <- (center.getX - dist until center.getX + dist);
          y <- (center.getY - dist until center.getY + dist)
        ) yield fullImage.get(x, y)

        val min = pixList.min
        val max = pixList.max
        // use the remaining color depth to allow the proper rendering of pixels brighter and dimmer than the allowed range.
        val outside = (255 - (max - min)) / 2
        ImageUtil.magnify(fullImage.toBufferedImage(ImageUtil.rgbColorMap(Config.EPIDImageColor), min - outside, max + outside), scale)
      }

    putGantryAngleAndDateAndTime(fullSize, 2)
    Config.applyWatermark(fullSize)
    applyOrientationWatermark(fullSize)
    drawCircleWithXAtCenterOfBB(new Point2d(0, 0), fullSize, scale)
    drawCirclesAtCenterOfPlan(new Point2d(0, 0), fullSize, scale)
    fullSize
  }

  private val dateOpt = BBbyEPIDAnnotateImages.epidDateText(al)

  private def putGantryAngleAndDateAndTime(bufImage: BufferedImage, fontScale: Int = 1) = {
    val graphics = ImageUtil.getGraphics(bufImage)
    val fontSize = textPointSize * fontScale

    if (dateOpt.isDefined) {
      val date = dateOpt.get
      val text = "G" + gantryAngleRounded + "   " + dateOpt.get
      ImageText.setFont(graphics, ImageText.DefaultFont, fontSize)
      val dim = ImageText.getTextDimensions(graphics, text)

      graphics.setColor(Color.black)
      graphics.fillRect(0, 0, dim.getWidth.toInt + fontSize * 2, dim.getHeight.toInt)
      graphics.setColor(Color.white)
      graphics.drawString(text, fontSize, fontSize)
    }
  }

  private def makeCloseupBufImg: BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 32

    // defines how much area around the bb should be used to make the close up image specified as a multiple of the bb size.
    val closeupArea = 10.0
    val closeup_mm = Config.EPIDBBPenumbra_mm * closeupArea

    // position of upper left corner of close up image in pixel coordinates
    val upperLeftCorner = {
      val p = trans.iso2Pix(trans.caxCenter_iso.getX - closeup_mm, trans.caxCenter_iso.getY - closeup_mm)
      new Point2d(p.getX, p.getY)
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
    putGantryAngleAndDateAndTime(closeupImage)
    Config.applyWatermark(closeupImage)
    applyOrientationWatermark(closeupImage)

    drawCircleWithXAtCenterOfBB(upperLeftCorner, closeupImage, scale)
    drawCirclesAtCenterOfPlan(upperLeftCorner, closeupImage, scale)

    closeupImage
  }

  val closeupBufImg = makeCloseupBufImg
  val fullBufImg = makeFullBufImg

}