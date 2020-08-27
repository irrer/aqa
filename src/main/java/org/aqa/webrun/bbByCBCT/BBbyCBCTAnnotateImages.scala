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
import org.aqa.Logging
import javax.vecmath.Point2d
import javax.imageio.ImageIO
import edu.umro.ImageUtil.Watermark

object BBbyCBCTAnnotateImages extends Logging {

  /** Factor to magnify area of interest image. */
  private val scale = 8

  private val textPointSize = 16

  case class ImageSet(fullImage: Seq[BufferedImage], areaOfInterest: Seq[BufferedImage]);

  private case class ImagePair(fullImage: BufferedImage, areaOfInterest: BufferedImage);

  private case class Centerlines(top: String, bottom: String, topBottomColor: Color, left: String, right: String, leftRightColor: Color);

  private def rotate = {
    ???
  }

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
    bbCenterUnscaled_pix: Point2d,
    xAxisName: String, yAxisName: String,
    xPlanOffset_mm: Double, yPlanOffset_mm: Double,
    originalImage: BufferedImage,
    centerlines: Centerlines,
    rotation: Int): ImagePair = {

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

    val bbCenter_pix: Point2d = {
      val bbCenterX_pix = {
        val xc = if (voxSizeX_mmOrig > voxSizeY_mmOrig)
          bbCenterUnscaled_pix.getX * (voxSizeX_mmOrig / voxSizeY_mmOrig)
        else
          bbCenterUnscaled_pix.getX
        (xc + 0.5) * scale
      }

      val bbCenterY_pix = {
        val yc = if (voxSizeY_mmOrig > voxSizeX_mmOrig)
          bbCenterUnscaled_pix.getY * (voxSizeY_mmOrig / voxSizeX_mmOrig)
        else
          bbCenterUnscaled_pix.getY
        (yc + 0.5) * scale
      }

      val bbUnrotated = new Point2d(bbCenterX_pix, bbCenterY_pix)

      val rotated = rotation match {
        case 0 => bbUnrotated
        case 1 => new Point2d((originalImage.getHeight * scale) - 1 - bbUnrotated.getY, bbUnrotated.getX)
        case _ => throw new RuntimeException("Unexpected rotation in makePair: " + rotation)
      }
      rotated
    }

    /** Size (width and height) of small image in pixels. */
    val cropSize_pix = {
      Config.CBCTBBPenumbra_mm * 2
      64 * scale
    }

    val planCenter_pix: Point2d = {
      /*
      val x = {
        val diff = -xPlanOffset_mm / voxSizeX_mm
        bbCenter_pix.getX + (diff * scale)
      }

      val y = {
        val diff = -yPlanOffset_mm / voxSizeY_mm
        bbCenter_pix.getY + (diff * scale)
      }

      val unrotated = new Point2d(x, y)

      val rotated = rotation match {
        case 0 => unrotated
        case 1 => new Point2d((originalImage.getHeight * scale) - 1 - unrotated.getY, unrotated.getX)
        case _ => throw new RuntimeException("Unexpected rotation in makePair: " + rotation)
      }

      rotated
      */

      new Point2d(100, 100)
    }

    // ---------------------------------------------------------------------------------

    def makeFullImage: BufferedImage = {
      val image = {
        val i = ImageUtil.magnify(originalImage, scale)
        rotation match {
          case 0 => i
          case 1 => ImageUtil.rotate90(i)
          case _ => throw new RuntimeException("Unexpected rotation in makeFullImage: " + rotation)
        }
      }

      val maxVoxSize = Math.max(voxSizeX_mm, voxSizeY_mm) // larger voxel dimension
      val minVoxSize = Math.min(voxSizeX_mm, voxSizeY_mm) // smaller voxel dimension
      val widthCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 2) / voxSizeX_mm) * scale //* (maxVoxSize / minVoxSize)
      }
      val heightCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 2) / voxSizeY_mm) * scale //* (maxVoxSize / minVoxSize)
      }

      // ---------------------------------------------------------------------------------

      /**
       * draw circle around BB
       */
      def drawCircle(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.white)

        graphics.drawOval(d2i(bbCenter_pix.getX - (widthCircle_pix / 2)), d2i(bbCenter_pix.getY - (heightCircle_pix / 2)), d2i(widthCircle_pix), d2i(heightCircle_pix))

        val radius = Math.sqrt((widthCircle_pix * widthCircle_pix) + (heightCircle_pix * heightCircle_pix)) / 4
        graphics.drawLine(d2i(bbCenter_pix.getX - radius + 1), d2i(bbCenter_pix.getY - radius), d2i(bbCenter_pix.getX + radius), d2i(bbCenter_pix.getY + radius))
        graphics.drawLine(d2i(bbCenter_pix.getX + radius - 1), d2i(bbCenter_pix.getY - radius), d2i(bbCenter_pix.getX - radius), d2i(bbCenter_pix.getY + radius))
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

          val x_pix = bbCenter_pix.getX - (textRect.getWidth / 2)
          val y_pix = bbCenter_pix.getY + (cropSize_pix / 2) - (Util.axisOffsetFromEdge + 15)

          graphics.drawString(text, d2i(x_pix), d2i(y_pix))
        }

        def drawY = {
          val text = deblank(yAxisName + " " + fmt(yPlanOffset_mm) + " mm")
          val textRect = ImageText.getTextDimensions(graphics, text)

          val x_pix = bbCenter_pix.getX - (cropSize_pix / 2) + 10
          val y_pix = bbCenter_pix.getY - (cropSize_pix / 3)

          graphics.drawString(text, d2i(x_pix), d2i(y_pix))
        }

        drawX
        drawY
      }

      // ---------------------------------------------------------------------------------

      def drawPlanCenter(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.green)

        val smallRadius = 4
        val largeRadius = smallRadius * 2

        graphics.drawOval(
          d2i(planCenter_pix.getX - smallRadius), d2i(planCenter_pix.getY - smallRadius),
          d2i(smallRadius * 2), d2i(smallRadius * 2))
        graphics.drawOval(
          d2i(planCenter_pix.getX - largeRadius), d2i(planCenter_pix.getY - largeRadius),
          d2i(largeRadius * 2), d2i(largeRadius * 2))
      }

      // ---------------------------------------------------------------------------------

      def drawPlanText(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.green)
        // show BB offset from plan
        val text = "P"

        val xOffset = if (bbCenter_pix.getX > planCenter_pix.getX) -1 else 1
        val yOffset = if (bbCenter_pix.getY > planCenter_pix.getY) -1 else 1

        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
        val textRect = ImageText.getTextDimensions(graphics, text)
        ImageText.drawTextCenteredAt(graphics, planCenter_pix.getX + xOffset * textPointSize, planCenter_pix.getY + yOffset * textPointSize, text)
      }

      drawCircle(image)
      drawOffsetNumbers(image)
      drawPlanCenter(image)
      drawPlanText(image)
      image
    }

    // ---------------------------------------------------------------------------------

    def cropImage(image: BufferedImage) = {
      val x = Math.max(0, d2i(bbCenter_pix.getX - (cropSize_pix / 2)))
      val y = Math.max(0, d2i(bbCenter_pix.getY - (cropSize_pix / 2)))

      val aoi = image.getSubimage(x, y, cropSize_pix, cropSize_pix)
      aoi
    }

    // ---------------------------------------------------------------------------------

    val full = makeFullImage
    val aoi = cropImage(full)

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
    logger.info("Annotating CBCT images for " + bbByCBCT)
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbctList) // the size of a voxel in mm

    val volTrans = new VolumeTranslator(runReq.cbctList)

    val centerUnscaled_vox = volTrans.mm2vox(origPosition)

    def xImagePair = makePair(
      bbByCBCT = bbByCBCT,
      voxSizeX_mmOrig = voxSize_mm(2), voxSizeY_mmOrig = voxSize_mm(1),
      bbCenterUnscaled_pix = new Point2d(centerUnscaled_vox.getZ, centerUnscaled_vox.getY),
      xAxisName = "Z", yAxisName = "Y",
      xPlanOffset_mm = bbByCBCT.err_mm.getZ, yPlanOffset_mm = bbByCBCT.err_mm.getY,
      originalImage = imageXYZ(0),
      centerlines = new Centerlines("A", "P", Color.red, "R", "L", Color.green),
      rotation = 1)

    def yImagePair = makePair(
      bbByCBCT,
      voxSize_mm(2), voxSize_mm(0),
      bbCenterUnscaled_pix = new Point2d(centerUnscaled_vox.getZ, centerUnscaled_vox.getX),
      "Z", "X",
      bbByCBCT.err_mm.getZ, bbByCBCT.err_mm.getX,
      imageXYZ(1),
      new Centerlines("A", "P", Color.red, "R", "L", Color.green),
      rotation = 1)

    def zImagePair = makePair(
      bbByCBCT,
      voxSize_mm(0), voxSize_mm(1),
      bbCenterUnscaled_pix = new Point2d(centerUnscaled_vox.getX, centerUnscaled_vox.getY),
      "X", "Y",
      bbByCBCT.err_mm.getX, bbByCBCT.err_mm.getY,
      imageXYZ(2),
      new Centerlines("A", "P", Color.red, "R", "L", Color.green),
      rotation = 0)

    // do annotation processing in parallel
    val pairList = Seq(xImagePair _, yImagePair _, zImagePair _).par.map(f => f()).toList

    val fullList = {
      val list = pairList.map(p => p.fullImage)

      val min = (list.map(i => i.getWidth) ++ list.map(i => i.getHeight)).min

      def trimImage(image: BufferedImage): BufferedImage = {
        if ((image.getWidth > min) || (image.getHeight > min)) {
          val x = Math.max((image.getWidth - min) / 2, 0)
          val y = Math.max((image.getHeight - min) / 2, 0)
          val w = Math.min(min, image.getWidth)
          val h = Math.min(min, image.getHeight)
          image.getSubimage(x, y, w, h)
        } else
          image
      }

      val resizedList = list.map(i => trimImage(i))
      resizedList.map(i => Config.applyWatermark(i))
      resizedList(0)
      resizedList
    }

    def applyOrientationLegend(image: BufferedImage, name: String, pctWidth: Double) = {
      val file = new File(Config.imageDirFile, name)
      Trace.trace("file: " + file.getAbsolutePath) // TODO rm
      val legend = ImageIO.read(file)
      val watermark = new Watermark(legend, false, true, pctWidth, 90.0)
      watermark.mark(image)
    }

    applyOrientationLegend(fullList(0), "HumanSagittal.png", 5.0)
    applyOrientationLegend(fullList(1), "HumanFrontal.png", 10.0)
    applyOrientationLegend(fullList(2), "HumanTransversal.png", 10.0)

    val imageSet = new ImageSet(
      fullList,
      pairList.map(p => p.areaOfInterest))
    imageSet
  }
}