package org.aqa.webrun.bbByCBCT

import org.aqa.db.BBbyCBCT
import java.awt.image.BufferedImage
import org.aqa.Util
import javax.vecmath.Point3d
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import org.aqa.Config
import java.io.File
import org.aqa.VolumeTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.ImageRegistration
import javax.vecmath.Matrix4d
import edu.umro.ImageUtil.ImageText
import java.awt.BasicStroke

object BBbyCBCTAnnotateImages {

  /** Factor to magnify area of interest image. */
  private val scale = 16

  private val textPointSize = 30

  case class ImageSet(fullImage: Seq[BufferedImage], areaOfInterest: Seq[BufferedImage]);

  private case class ImagePair(fullImage: BufferedImage, areaOfInterest: BufferedImage);

  /**
   * Make a pair of images for a single axis orientation.
   *
   * @param bbByCBCT Results of analysis, stored in database
   *
   * @param voxSizeX_mm, voxSizeY_mm Size of X and Y voxels in generated image for this orientation
   *
   * @param centerX_pix, centerY_pix Center of X and Y voxels in generated image for this orientation
   *
   * @param xAxisName, yAxisName Names of X and Y axis
   *
   * @param originalImage Image for this orientation
   */
  private def makePair(
    bbByCBCT: BBbyCBCT,
    voxSizeX_mmOrig: Double, voxSizeY_mmOrig: Double,
    centerUnscaledX_pix: Double, centerUnscaledY_pix: Double,
    xAxisName: String, yAxisName: String,
    xPlanOffset_mm: Double, yPlanOffset_mm: Double,
    originalImage: BufferedImage): ImagePair = {

    def d2i(d: Double) = d.round.toInt

    val min_pix = Math.min(voxSizeX_mmOrig, voxSizeY_mmOrig)
    val max_pix = Math.max(voxSizeX_mmOrig, voxSizeY_mmOrig)

    val voxSizeX_mm = {
      //if (voxSizeX_mmOrig > voxSizeY_mmOrig) voxSizeX_mmOrig / voxSizeY_mmOrig else voxSizeX_mmOrig
      Math.min(voxSizeX_mmOrig, voxSizeY_mmOrig)
    }

    val voxSizeY_mm = {
      //if (voxSizeX_mmOrig > voxSizeY_mmOrig) voxSizeX_mmOrig else voxSizeX_mmOrig / voxSizeY_mmOrig
      voxSizeX_mm
    }

    val centerX_pix = {
      val xc = if (voxSizeX_mmOrig > voxSizeY_mmOrig)
        centerUnscaledX_pix * (voxSizeX_mmOrig / voxSizeY_mmOrig)
      else
        centerUnscaledX_pix
      (xc + 0.5) * scale
    }

    val centerY_pix = {
      val yc = if (voxSizeY_mmOrig > voxSizeX_mmOrig)
        centerUnscaledY_pix * (voxSizeY_mmOrig / voxSizeX_mmOrig)
      else
        centerUnscaledY_pix
      (yc + 0.5) * scale
    }

    /** Size (width and height) of small image in pixels. */
    val cropSize_pix = 500

    // ---------------------------------------------------------------------------------

    def makeFullImage: BufferedImage = {
      val image = ImageUtil.magnify(originalImage, scale)

      val maxVoxSize = Math.max(voxSizeX_mm, voxSizeY_mm) // larger voxel dimension
      val minVoxSize = Math.min(voxSizeX_mm, voxSizeY_mm) // smaller voxel dimension
      val widthCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 2) / voxSizeX_mm) * scale //* (maxVoxSize / minVoxSize)
      }
      val heightCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 2) / voxSizeY_mm) * scale //* (maxVoxSize / minVoxSize)
      }

      /**
       * draw circle around BB
       */
      def drawCircle(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.white)

        graphics.drawOval(d2i(centerX_pix - (widthCircle_pix / 2)), d2i(centerY_pix - (heightCircle_pix / 2)), d2i(widthCircle_pix), d2i(heightCircle_pix))

        val radius = Math.sqrt((widthCircle_pix * widthCircle_pix) + (heightCircle_pix * heightCircle_pix)) / 4
        graphics.drawLine(d2i(centerX_pix - radius + 1), d2i(centerY_pix - radius), d2i(centerX_pix + radius), d2i(centerY_pix + radius))
        graphics.drawLine(d2i(centerX_pix + radius - 1), d2i(centerY_pix - radius), d2i(centerX_pix - radius), d2i(centerY_pix + radius))
      }

      // ---------------------------------------------------------------------------------

      /**
       * Draw the offset for each axis.
       */
      def drawOffsetNumbers(zImage: BufferedImage): Unit = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.white)
        def fmt(d: Double) = d.formatted("%8.2f")
        def deblank(text: String) = text.replaceAll("   *", " ")
        // show BB offset from plan

        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)

        def drawX = {
          val text = deblank(xAxisName + " " + fmt(xPlanOffset_mm) + " mm")
          val textRect = ImageText.getTextDimensions(graphics, text)

          val x_pix = centerX_pix - (textRect.getWidth / 2)
          val y_pix = centerY_pix + (cropSize_pix / 2) - (Util.axisOffsetFromEdge + 15)

          graphics.drawString(text, d2i(x_pix), d2i(y_pix))
        }

        def drawY = {
          val text = deblank(yAxisName + " " + fmt(yPlanOffset_mm) + " mm")
          val textRect = ImageText.getTextDimensions(graphics, text)

          val x_pix = centerX_pix - (cropSize_pix / 2) + 10
          val y_pix = centerY_pix - (cropSize_pix / 3)

          graphics.drawString(text, d2i(x_pix), d2i(y_pix))
        }

        drawX
        drawY
      }

      // ---------------------------------------------------------------------------------

      def drawPlanCenter(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.red)

        val planXcenter = {
          val diff = xPlanOffset_mm / voxSizeX_mm
          centerX_pix + (diff * scale)
        }

        val planYcenter = {
          val diff = yPlanOffset_mm / voxSizeY_mm
          centerY_pix + (diff * scale)
        }

        graphics.drawLine(
          d2i(planXcenter), d2i(planYcenter - scale),
          d2i(planXcenter), d2i(planYcenter + scale))
        graphics.drawLine(
          d2i(planXcenter - scale), d2i(planYcenter),
          d2i(planXcenter + scale), d2i(planYcenter))
      }

      // ---------------------------------------------------------------------------------

      def drawPlanText(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.red)
        // show BB offset from plan
        val text = "'+' is Plan Center"

        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
        val textRect = ImageText.getTextDimensions(graphics, text)
        ImageText.drawTextOffsetFrom(graphics, centerX_pix, centerY_pix - (heightCircle_pix / 2), text, 90)
      }

      drawCircle(image)
      drawOffsetNumbers(image)
      drawPlanCenter(image)
      drawPlanText(image)
      image
    }

    // ---------------------------------------------------------------------------------

    def cropImage(image: BufferedImage) = {
      val x = Math.max(0, d2i(centerX_pix - (cropSize_pix / 2)))
      val y = Math.max(0, d2i(centerY_pix - (cropSize_pix / 2)))

      val aoi = image.getSubimage(x, y, cropSize_pix, cropSize_pix)
      aoi
    }

    // ---------------------------------------------------------------------------------

    val full = makeFullImage
    val aoi = cropImage(full)
    Config.applyWatermark(full)
    Config.applyWatermark(aoi)

    Util.addAxisLabels(aoi, "", "", Color.white, true, false, false, true) // top, bottom, left, right arrow heads

    new ImagePair(full, aoi)
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
   * @param origPosition Original XYZ position in mm in the original frame of reference.
   */
  def annotate(bbByCBCT: BBbyCBCT, imageXYZ: Seq[BufferedImage], runReq: BBbyCBCTRunReq, origPosition: Point3d): ImageSet = {
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbctList) // the size of a voxel in mm

    val volTrans = new VolumeTranslator(runReq.cbctList)

    val centerUnscaled_vox = volTrans.mm2vox(origPosition)

    def xImagePair = makePair(
      bbByCBCT,
      voxSize_mm(2), voxSize_mm(1),
      centerUnscaled_vox.getZ, centerUnscaled_vox.getY,
      "Z", "Y",
      bbByCBCT.err_mm.getZ, bbByCBCT.err_mm.getY,
      imageXYZ(0))

    def yImagePair = makePair(
      bbByCBCT,
      voxSize_mm(2), voxSize_mm(0),
      centerUnscaled_vox.getZ, centerUnscaled_vox.getX,
      "Z", "X",
      bbByCBCT.err_mm.getZ, bbByCBCT.err_mm.getX,
      imageXYZ(1))

    bbByCBCT.cbctZ_mm - bbByCBCT.rtplanZ_mm

    def zImagePair = makePair(
      bbByCBCT,
      voxSize_mm(0), voxSize_mm(1),
      centerUnscaled_vox.getX, centerUnscaled_vox.getY,
      "X", "Y",
      bbByCBCT.err_mm.getX, bbByCBCT.err_mm.getY,
      imageXYZ(2))

    val pairList = Seq(xImagePair _, yImagePair _, zImagePair _).par.map(f => f()).toList

    val imageSet = new ImageSet(
      pairList.map(p => p.fullImage),
      pairList.map(p => p.areaOfInterest))
    //  Seq(xImagePair.fullImage, yImagePair.fullImage, zImagePair.fullImage),
    //  Seq(xImagePair.areaOfInterest, yImagePair.areaOfInterest, zImagePair.areaOfInterest))
    imageSet
  }
}