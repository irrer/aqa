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
  private def drawPlusAtCenterOfPlan(center: Point2D.Double, bufImage: BufferedImage, scale: Int) = {
    val graphics = ImageUtil.getGraphics(bufImage)
    ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
    Trace.trace("center: " + center)

    val radH = d2i(radius_pix * 1.5 * scale)

    val xCenter = (center.getX * scale).round.toInt
    val yCenter = (center.getY * scale).round.toInt

    graphics.setColor(Color.white)
    graphics.drawLine(xCenter - 1, yCenter - radH, xCenter - 1, yCenter + radH)
    graphics.drawLine(xCenter + 1, yCenter - radH, xCenter + 1, yCenter + radH)
    graphics.drawLine(xCenter - radH, yCenter - 1, xCenter + radH, yCenter - 1)
    graphics.drawLine(xCenter - radH, yCenter + 1, xCenter + radH, yCenter + 1)

    graphics.setColor(Color.black)
    graphics.drawLine(xCenter, yCenter - radH, xCenter, yCenter + radH)
    graphics.drawLine(xCenter - radH, yCenter, xCenter + radH, yCenter)

    ImageText.drawTextOffsetFrom(graphics, xCenter, yCenter + circleRadius_pix(scale) + textPointSize, "+ is plan center", 270)
  }

  private def makeFullBufImg: BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 4

    val fullSize = ImageUtil.magnify(fullImage.toDeepColorBufferedImage(0.05), scale)
    Config.applyWatermark(fullSize)

    val pixCenter = trans.iso2Pix(trans.isoCenter)

    drawCircleWithXAtCenterOfBB(new Point2D.Double(0, 0), fullSize, scale)
    drawPlusAtCenterOfPlan(pixCenter, fullSize, scale)
    fullSize
  }

  private def makeCloseupBufImg: BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 32

    // defines how much area around the bb should be used to make the close up imate
    val closeupScale = 5.0

    // position of upper left corner of close up image in pixel coordinates
    val upperLeftCorner = {
      if (bbLoc_mmIso.isDefined) {
        trans.iso2Pix(bbLoc_mmIso.get.getX - Config.EPIDBBPenumbra_mm * closeupScale, -Config.EPIDBBPenumbra_mm * closeupScale)
      } else {
        // if no BB found, then just make a close up of the center of the image
        trans.iso2Pix(-Config.EPIDBBPenumbra_mm * closeupScale, -Config.EPIDBBPenumbra_mm * closeupScale)
      }
    }

    val center = {
      val pixCenter = trans.iso2Pix(trans.isoCenter)
      new Point2D.Double((pixCenter.getX - upperLeftCorner.getX), (pixCenter.getY - upperLeftCorner.getY))
    }

    Trace.trace("closeup center: " + center)

    // define rectangle for copying just the middle of the image
    val closeupRect = {
      val x = d2i(upperLeftCorner.getX)
      val y = d2i(upperLeftCorner.getY)
      val w = d2i(trans.iso2PixDistX(Config.EPIDBBPenumbra_mm * closeupScale * 2))
      val h = d2i(trans.iso2PixDistY(Config.EPIDBBPenumbra_mm * closeupScale * 2))
      new Rectangle(x, y, w, h)
    }

    val closeupImage = ImageUtil.magnify(fullImage.getSubimage(closeupRect).toBufferedImage(Config.EPIDImageColor), scale)
    Config.applyWatermark(closeupImage)

    drawCircleWithXAtCenterOfBB(upperLeftCorner, closeupImage, scale)
    drawPlusAtCenterOfPlan(center, closeupImage, scale)

    closeupImage
  }

  val fullBufImg = makeFullBufImg
  val closeupBufImg = makeCloseupBufImg

}