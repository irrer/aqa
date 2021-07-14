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

package org.aqa.webrun.bbByCBCT

import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.OrientationWatermark
import org.aqa.Util
import org.aqa.db.BBbyCBCT

import java.awt.BasicStroke
import java.awt.Color
import java.awt.image.BufferedImage
import java.text.SimpleDateFormat
import java.util.Date
import javax.vecmath.Point2d
import javax.vecmath.Point3d

object BBbyCBCTAnnotateImages extends Logging {

  /** Factor to magnify area of interest image. */
  private val scale = 8

  private val textPointSize = Util.d2i(scale * 2.5)

  case class ImageSet(fullImage: Seq[BufferedImage], areaOfInterest: Seq[BufferedImage]) {}

  private case class ImagePair(fullImage: BufferedImage, areaOfInterest: BufferedImage) {}

  private case class Centerlines(top: String, bottom: String, topBottomColor: Color, left: String, right: String, leftRightColor: Color) {}

  /**
    * Make a pair of images for a single axis orientation.
    *
    * @param originalImage Image for this orientation
    *
    * @param vertCenterline Color of vertical center line.
    *
    * @param horzCenterline Color of horizontal center line.
    *
    */
  private def makePair(
      voxSizeX_mmOrig: Double,
      voxSizeY_mmOrig: Double,
      bb_pix: Point2d,
      rtplanOrigin_pix: Point2d,
      originalImage: BufferedImage,
      mapImage: BufferedImage => BufferedImage,
      mapPoint: (Point2d, Int, Int) => Point2d,
      vertCenterline: Color,
      horzCenterline: Color
  ): BufferedImage = {

    Trace.trace("voxSizeX_mmOrig: " + voxSizeX_mmOrig + "    voxSizeY_mmOrig: " + voxSizeY_mmOrig)

    def d2i(d: Double) = d.round.toInt

    val voxSizeX_mm = {
      //if (voxSizeX_mmOrig > voxSizeY_mmOrig) voxSizeX_mmOrig / voxSizeY_mmOrig else voxSizeX_mmOrig
      Math.min(voxSizeX_mmOrig, voxSizeY_mmOrig)
    }

    val voxSizeY_mm = {
      //if (voxSizeX_mmOrig > voxSizeY_mmOrig) voxSizeX_mmOrig else voxSizeX_mmOrig / voxSizeY_mmOrig
      voxSizeX_mm
    }

    def scaleAndRotatePoint(unscaledAndUnrotated: Point2d): Point2d = {
      val centerX_pix = {
        val xc =
          if (voxSizeX_mmOrig > voxSizeY_mmOrig)
            unscaledAndUnrotated.getX * (voxSizeX_mmOrig / voxSizeY_mmOrig)
          else
            unscaledAndUnrotated.getX
        //(xc + 0.5) * scale
        Util.scalePixel(scale, xc)
      }

      val centerY_pix = {
        val yc =
          if (voxSizeY_mmOrig > voxSizeX_mmOrig)
            unscaledAndUnrotated.getY * (voxSizeY_mmOrig / voxSizeX_mmOrig)
          else
            unscaledAndUnrotated.getY
        //(yc + 0.5) * scale
        Util.scalePixel(scale, yc)
      }

      val center = new Point2d(centerX_pix, centerY_pix)

      val p = mapPoint(center, originalImage.getWidth * scale, originalImage.getHeight * scale)

      p
    }

    val bbCenter_pix = scaleAndRotatePoint(bb_pix)

    val planCenter_pix = scaleAndRotatePoint(rtplanOrigin_pix)

    Trace.trace("bbCenter_pix: " + bbCenter_pix)
    Trace.trace("planCenter_pix: " + planCenter_pix)

    // ---------------------------------------------------------------------------------

    def makeFullImage: BufferedImage = {
      val image = ImageUtil.magnify(mapImage(originalImage), scale)

      val widthCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 10) / voxSizeX_mm) * scale //* (maxVoxSize / minVoxSize)
      }
      val heightCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 10) / voxSizeY_mm) * scale //* (maxVoxSize / minVoxSize)
      }

      // ---------------------------------------------------------------------------------

      /**
        * draw circle around BB
        */
      def drawCircle(): Unit = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.white)

        graphics.drawOval(d2i(bbCenter_pix.getX - (widthCircle_pix / 2)), d2i(bbCenter_pix.getY - (heightCircle_pix / 2)), d2i(widthCircle_pix), d2i(heightCircle_pix))

        val radius = Math.sqrt((widthCircle_pix * widthCircle_pix) + (heightCircle_pix * heightCircle_pix)) / 4
        graphics.drawLine(d2i(bbCenter_pix.getX - radius + 1), d2i(bbCenter_pix.getY - radius), d2i(bbCenter_pix.getX + radius), d2i(bbCenter_pix.getY + radius))
        graphics.drawLine(d2i(bbCenter_pix.getX + radius - 1), d2i(bbCenter_pix.getY - radius), d2i(bbCenter_pix.getX - radius), d2i(bbCenter_pix.getY + radius))
      }

      // ---------------------------------------------------------------------------------

      def drawPlanText(): Unit = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.red)
        // show BB offset from plan
        val text = "P"
        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)

        val dist = bbCenter_pix.distance(planCenter_pix)
        val distP = Math.sqrt(2.0) * textPointSize
        val ratio = distP / dist
        val x = planCenter_pix.getX + (planCenter_pix.getX - bbCenter_pix.getX) * ratio
        val y = planCenter_pix.getY + (planCenter_pix.getY - bbCenter_pix.getY) * ratio
        ImageText.drawTextCenteredAt(graphics, x, y, text)
      }

      // ---------------------------------------------------------------------------------

      def drawCrossLines(): Unit = {
        val graphics = ImageUtil.getGraphics(image)
        val dashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(6, 2), 0)
        graphics.setStroke(dashedLine)

        graphics.setColor(vertCenterline)
        graphics.drawLine(d2i(bbCenter_pix.getX), 0, d2i(bbCenter_pix.getX), image.getHeight - 1)

        graphics.setColor(horzCenterline)
        graphics.drawLine(0, d2i(bbCenter_pix.getY), image.getWidth - 1, d2i(bbCenter_pix.getY))
      }

      // ---------------------------------------------------------------------------------

      drawCircle()
      //Util.drawPlanCenter(image, planCenter_pix)
      Util.drawTarget(planCenter_pix, image, scale * 5) // draw plan center
      drawPlanText()
      drawCrossLines()
      image
    }

    // ---------------------------------------------------------------------------------

    val full = makeFullImage
    full
  }

  /**
    * Annotate the images for the user.
    *
    * @param bbByCBCT Results of analysis; stored in the database.
    *
    * @param imageXYZ Images created from X, Y, and Z axis views respectively.
    *
    * @param runReq DICOM files.
    *
    * @param bb_vox BB position in voxels in the the CBCT voxel space.
    *
    * @param rtplanOrigin_vox RTPLAN origin in voxels in the the CBCT voxel space.
    */
  def annotate(bbByCBCT: BBbyCBCT, imageXYZ: Seq[BufferedImage], runReq: BBbyCBCTRunReq, bb_vox: Point3d, rtplanOrigin_vox: Point3d, date: Date): ImageSet = {
    logger.info("Annotating CBCT images for " + bbByCBCT)
    val start = System.currentTimeMillis()
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbctList) // the size of a voxel in mm
    logger.info("Annotating CBCT images with voxels sized in mm: " + voxSize_mm)

    // sagittal
    def xImagePair =
      makePair(
        voxSizeX_mmOrig = voxSize_mm.getZ,
        voxSizeY_mmOrig = voxSize_mm.getY,
        bb_pix = new Point2d(bb_vox.getZ, bb_vox.getY),
        rtplanOrigin_pix = new Point2d(rtplanOrigin_vox.getZ, rtplanOrigin_vox.getY),
        originalImage = imageXYZ.head,
        mapImage = (i: BufferedImage) => ImageUtil.mirrorVertically(ImageUtil.rotate90(i)),
        mapPoint = (p: Point2d, w: Int, h: Int) => new Point2d(h - 1 - p.getY, w - 1 - p.getX),
        vertCenterline = Color.green,
        horzCenterline = Color.blue
      )

    // frontal
    def yImagePair =
      makePair(
        voxSizeX_mmOrig = voxSize_mm.getZ,
        voxSizeY_mmOrig = voxSize_mm.getX,
        bb_pix = new Point2d(bb_vox.getZ, bb_vox.getX),
        rtplanOrigin_pix = new Point2d(rtplanOrigin_vox.getZ, rtplanOrigin_vox.getX),
        originalImage = imageXYZ(1),
        mapImage = (i: BufferedImage) => ImageUtil.rotate270(i),
        mapPoint = (p: Point2d, w: Int, _: Int) => new Point2d(p.getY, w - 1 - p.getX),
        vertCenterline = Color.red,
        horzCenterline = Color.blue
      )

    // transversal
    def zImagePair =
      makePair(
        voxSizeX_mmOrig = voxSize_mm.getX,
        voxSizeY_mmOrig = voxSize_mm.getY,
        bb_pix = new Point2d(bb_vox.getX, bb_vox.getY),
        rtplanOrigin_pix = new Point2d(rtplanOrigin_vox.getX, rtplanOrigin_vox.getY),
        originalImage = imageXYZ(2),
        mapImage = (i: BufferedImage) => i, // ImageUtil.mirrorVertically(i),
        mapPoint = (p: Point2d, _: Int, _: Int) => p, // new Point2d(p.getX, h - 1 - p.getY),
        vertCenterline = Color.red,
        horzCenterline = Color.green
      )

    // do annotation processing in parallel
    val imageList = Seq(xImagePair _, yImagePair _, zImagePair _).par.map(f => f()).toList

    val aoiList = {

      val min = (imageList.map(i => i.getWidth) ++ imageList.map(i => i.getHeight)).min

      def trimImage(image: BufferedImage): BufferedImage = {
        if ((image.getWidth > min) || (image.getHeight > min)) {
          val x = Math.max((image.getWidth - min) / 2, 0)
          val y = Math.max((image.getHeight - min) / 2, 0)
          val w = Math.min(min, image.getWidth)
          val h = Math.min(min, image.getHeight)
          val aoi = {
            val tmp = image.getSubimage(x, y, w, h)
            val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
            for (xx <- 0 until w; yy <- 0 until h)
              img.setRGB(xx, yy, tmp.getRGB(xx, yy)) // copy the pixels so that they are not shared with the full sized image
            img
          }
          aoi
        } else
          image
      }

      val resizedList = imageList.map(i => trimImage(i))
      resizedList.foreach(i => Config.applyWatermark(i))
      resizedList
    }

    def applyOrientationLegend(image: BufferedImage, label: String, ow: OrientationWatermark.Value, top: String, bottom: String, left: String, right: String): Unit = {

      def label4sides(): Unit = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.red)
        val pointSize = (textPointSize * 2.5).round.toInt
        ImageText.setFont(graphics, ImageText.DefaultFont, pointSize)

        ImageText.drawTextCenteredAt(graphics, image.getWidth / 2, pointSize, top)
        ImageText.drawTextCenteredAt(graphics, image.getWidth / 2, image.getHeight - pointSize, bottom)
        ImageText.drawTextCenteredAt(graphics, pointSize, image.getHeight / 2, left)
        ImageText.drawTextCenteredAt(graphics, image.getWidth - pointSize, image.getHeight / 2, right)
      }

      def applyLabel(): Unit = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.white)
        val pointSize = (textPointSize * 2.5).round.toInt
        ImageText.setFont(graphics, ImageText.DefaultFont, pointSize)
        val dateFormat = new SimpleDateFormat("M/d/yyyy h:mm aa")
        val text = label + " - CBCT - " + dateFormat.format(date)
        graphics.drawString(text, 3, pointSize)
      }

      OrientationWatermark.mark(ow, image)
      label4sides()
      applyLabel()
    }

    applyOrientationLegend(imageList.head, "Sagittal", OrientationWatermark.SagittalRight, "H", "F", "P", "A")
    applyOrientationLegend(imageList(1), "Frontal", OrientationWatermark.Frontal, "H", "F", "R", "L")
    applyOrientationLegend(imageList(2), "Transversal", OrientationWatermark.Transversal, "A", "P", "R", "L")

    applyOrientationLegend(aoiList.head, "Sagittal", OrientationWatermark.SagittalRight, "H", "F", "P", "A")
    applyOrientationLegend(aoiList(1), "Frontal", OrientationWatermark.Frontal, "H", "F", "R", "L")
    applyOrientationLegend(aoiList(2), "Transversal", OrientationWatermark.Transversal, "A", "P", "R", "L")

    val imageSet = ImageSet(imageList, aoiList)

    logger.info("Finished annotating CBCT images in " + (System.currentTimeMillis() - start) + " ms for " + bbByCBCT)
    imageSet
  }
}
