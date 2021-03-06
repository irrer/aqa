/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.bbByEpid

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.OrientationWatermark
import org.aqa.Util

import java.awt.Color
import java.awt.Rectangle
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.text.SimpleDateFormat
import javax.vecmath.Point2d
import javax.vecmath.Point2i

object BBbyEPIDAnnotateImages {
  // get the date and time via either the Content or AcquisitionF
  def epidDateText(attrList: AttributeList): Option[String] = {
    val dateTimeList = Seq((TagFromName.ContentDate, TagFromName.ContentTime), (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime))
    val dateTime = dateTimeList.flatMap(dt => DicomUtil.getTimeAndDate(attrList, dt._1, dt._2)).headOption
    val dateFormat = new SimpleDateFormat("M/d/yyyy H:mm:ss")
    if (dateTime.isDefined) Some(dateFormat.format(dateTime.get)) else None
  }
}

/**
  * Create user friendly images and annotate them.
  */
class BBbyEPIDAnnotateImages(al: AttributeList, description: Option[String], bbCenter_pix: Option[Point2d]) {

  private val trans = new IsoImagePlaneTranslator(al)
  val gantryAngleRounded: Int = Util.angleRoundedTo90(Util.gantryAngle(al))

  private def d2D(d: Point2d) = new Point2D.Double(d.getX, d.getY)

  private val planCenter_pix = trans.caxCenter_pix
  //Trace.trace(ga + "   bbCenter_pix: " + bbCenter_pix + "    caxCenter_pix: " + planCenter_pix + "    diff: " + (bbCenter_pix.get.getX - planCenter_pix.getX) + ", " + (bbCenter_pix.get.getY - planCenter_pix.getY))

  private val textPointSize = 30

  private def d2i(d: Double) = d.round.toInt

  // The circle drawn around the BB should be this many times the BB's size
  private val circleRadiusScale = 2.0

  private val fullImage = new DicomImage(al)
  private val radius_pix = trans.iso2PixDistX(Config.EPIDBBPenumbra_mm) // radius of the BB in pixels

  val offsetText: String = {
    if (description.isDefined)
      description.get.trim
    else
      "Offset Not Available"
  }

  private def circleRadius_pix(scale: Int) = radius_pix * circleRadiusScale * scale

  private def applyOrientationWatermark(image: BufferedImage): Unit = {
    gantryAngleRounded match {
      case 0   => OrientationWatermark.mark(OrientationWatermark.Frontal, image)
      case 90  => OrientationWatermark.mark(OrientationWatermark.SagittalLeft, image)
      case 180 => OrientationWatermark.mark(OrientationWatermark.Posterior, image)
      case 270 => OrientationWatermark.mark(OrientationWatermark.SagittalRight, image)
      case _   =>
    }
  }

  /**
    *  draw a circle with an X in it centered around the BB
    *
    *  @param offset Offset from original image
    *
    *  @param bufImage Draw in this image
    *
    *  @param scale Scale of bufImage compared to original
    */
  private def drawCircleWithXAtCenterOfBB(offset: Point2d, bufImage: BufferedImage, scale: Int): Unit = {
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

      graphics.drawLine(d2i(psX - lineOffset), d2i(psY - lineOffset), d2i(psX + lineOffset), d2i(psY + lineOffset))

      graphics.drawLine(d2i(psX + lineOffset), d2i(psY - lineOffset), d2i(psX - lineOffset), d2i(psY + lineOffset))

      ImageText.drawTextCenteredAt(graphics, psX, y - circleRadius_pix(scale) - 2 * textPointSize, "X is BB center")

      ImageText.drawTextCenteredAt(graphics, psX, y - circleRadius_pix(scale) - textPointSize, offsetText)
    }
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
        val pixList =
          for (
            x <- center.getX - dist until center.getX + dist;
            y <- center.getY - dist until center.getY + dist
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
    //drawCirclesAtCenterOfPlan(new Point2d(0, 0), fullSize, scale)

    val scaledPlanCenter = new Point2d(Util.scalePixel(scale, planCenter_pix.getX), Util.scalePixel(scale, planCenter_pix.getY))
    Util.drawTarget(scaledPlanCenter, fullSize, scale * 2)

    fullSize
  }

  private val dateOpt = BBbyEPIDAnnotateImages.epidDateText(al)

  private def putGantryAngleAndDateAndTime(bufImage: BufferedImage, fontScale: Int = 1): Unit = {
    val graphics = ImageUtil.getGraphics(bufImage)
    val fontSize = textPointSize * fontScale

    if (dateOpt.isDefined) {
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
    //drawCirclesAtCenterOfPlan(upperLeftCorner, closeupImage, scale)
    val scaledPlanCenter = new Point2d(Util.scalePixel(scale, planCenter_pix.getX - upperLeftCorner.getX), Util.scalePixel(scale, planCenter_pix.getY - upperLeftCorner.getY))

    Util.drawTarget(scaledPlanCenter, closeupImage, scale)

    closeupImage
  }

  val closeupBufImg: BufferedImage = makeCloseupBufImg
  val fullBufImg: BufferedImage = makeFullBufImg

}
