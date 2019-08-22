package org.aqa.webrun.bbByEpid

import com.pixelmed.dicom.AttributeList
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.DicomImage
import java.awt.geom.Point2D
import org.aqa.IsoImagePlaneTranslator
import java.awt.Color
import java.awt.Rectangle
import org.aqa.Config
import edu.umro.ImageUtil.ImageUtil

object BBbyEPIDAnnotate {

  case class ImageSet(fullSize: BufferedImage, detail: BufferedImage);

  private def d2i(d: Double) = d.round.toInt

  private def makeFullBufImg(al: AttributeList, bbLoc_mm: Point2D.Double): BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 4

    val trans = new IsoImagePlaneTranslator(al)
    val bbLoc_pix = trans.iso2Pix(bbLoc_mm)
    val fullImage = new DicomImage(al)
    val radius_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm)
    val bbMaxValue = {
      val bbRect = new Rectangle(d2i(bbLoc_pix.getX - radius_pix), d2i(bbLoc_pix.getY - radius_pix), d2i(radius_pix * 2), d2i(radius_pix * 2))
      fullImage.getSubimage(bbRect).maxPixelValue
    }

    val depthLimitedImage = new DicomImage(fullImage.pixelData.map(row => row.map(p => Math.min(p, bbMaxValue))))

    val fullSize = ImageUtil.magnify(depthLimitedImage.toBufferedImage(Config.EPIDImageColor), scale)
    //val fullSize = ImageUtil.magnify(depthLimitedImage.toDeepColorBufferedImage(0.05), scale)

    // draw a circle with an X in it centered around the BB
    val graphics = ImageUtil.getGraphics(fullSize)
    graphics.setColor(Color.white)

    val circleRadius = radius_pix * scale * 3

    val psX = (bbLoc_pix.getX * scale) + (scale / 2)
    val psY = (bbLoc_pix.getY * scale) + (scale / 2)
    val x = psX - circleRadius
    val y = psY - circleRadius
    val w = circleRadius * 2
    val h = w

    val lineOffset = circleRadius / Math.sqrt(2.0)

    graphics.drawOval(d2i(x), d2i(y), d2i(w), d2i(h))

    graphics.drawLine(
      d2i(psX - lineOffset), d2i(psY - lineOffset),
      d2i(psX + lineOffset), d2i(psY + lineOffset))

    graphics.drawLine(
      d2i(psX + lineOffset), d2i(psY - lineOffset),
      d2i(psX - lineOffset), d2i(psY + lineOffset))

    // draw a red + in the middle of the image
    graphics.setColor(Color.red)
    val radH = d2i((radius_pix / 2) * scale)
    val xCenter = fullSize.getWidth / 2
    val yCenter = fullSize.getHeight / 2
    graphics.drawLine(xCenter, yCenter - radH, xCenter, yCenter + radH)
    graphics.drawLine(xCenter - radH, yCenter, xCenter + radH, yCenter)

    fullSize
  }

  private def makeDetailBufImg(al: AttributeList, bbLoc_mm: Point2D.Double): BufferedImage = {
    /** Magnify the image by this scale. */
    val scale = 16

    val trans = new IsoImagePlaneTranslator(al)
    val bbLoc_pix = trans.iso2Pix(bbLoc_mm)
    val fullImage = new DicomImage(al)

    ???
  }

  def annotate(al: AttributeList, bbLoc_mm: Point2D.Double): ImageSet = {
    val fullBufImg = makeFullBufImg(al, bbLoc_mm)
    new ImageSet(fullBufImg, fullBufImg)
  }
}