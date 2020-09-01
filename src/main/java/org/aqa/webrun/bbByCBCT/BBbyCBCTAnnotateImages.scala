package org.aqa.webrun.bbByCBCT

import org.aqa.db.BBbyCBCT
import java.awt.image.BufferedImage
import org.aqa.Util
import javax.vecmath.Point3d
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import org.aqa.Config
import java.io.File
import java.util.Date
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
import java.text.SimpleDateFormat

object BBbyCBCTAnnotateImages extends Logging {

  /** Factor to magnify area of interest image. */
  private val scale = 6

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
   *
   * @param vertCenterline Color of vertical center line.
   *
   * @param horzCenterline Color of horizontal center line.
   *
   * @param rotation Number of degrees to rotate image clockwise.
   */
  private def makePair(
    voxSizeX_mmOrig: Double, voxSizeY_mmOrig: Double,
    bb_pix: Point2d,
    rtplanOrigin_pix: Point2d,
    originalImage: BufferedImage,
    vertCenterline: Color,
    horzCenterline: Color,
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

    def scaleAndRotatePoint(unscaledAndUnrotated: Point2d): Point2d = {
      val centerX_pix = {
        val xc = if (voxSizeX_mmOrig > voxSizeY_mmOrig)
          unscaledAndUnrotated.getX * (voxSizeX_mmOrig / voxSizeY_mmOrig)
        else
          unscaledAndUnrotated.getX
        (xc + 0.5) * scale
      }

      val centerY_pix = {
        val yc = if (voxSizeY_mmOrig > voxSizeX_mmOrig)
          unscaledAndUnrotated.getY * (voxSizeY_mmOrig / voxSizeX_mmOrig)
        else
          unscaledAndUnrotated.getY
        (yc + 0.5) * scale
      }

      val rotated = rotation match {
        case 0 => new Point2d(centerX_pix, centerY_pix)
        case 90 => new Point2d((originalImage.getHeight * scale) - 1 - centerY_pix, centerX_pix)
        case 180 => new Point2d((originalImage.getWidth * scale) - centerX_pix, (originalImage.getHeight * scale) - centerY_pix)
        case _ => throw new RuntimeException("Unexpected rotation in makePair: " + rotation)
      }
      rotated
    }

    val bbCenter_pix = scaleAndRotatePoint(bb_pix)

    val planCenter_pix = scaleAndRotatePoint(rtplanOrigin_pix)

    Trace.trace("bbCenter_pix: " + bbCenter_pix)
    Trace.trace("planCenter_pix: " + planCenter_pix)

    /** Size (width and height) of small image in pixels. */
    val cropSize_pix = {
      Config.CBCTBBPenumbra_mm * 2
      64 * scale
    }

    // ---------------------------------------------------------------------------------

    def makeFullImage: BufferedImage = {
      val image = {
        val i = ImageUtil.magnify(originalImage, scale)
        rotation match {
          case 0 => i
          case 90 => ImageUtil.rotate90(i)
          case 180 => ImageUtil.rotate180(i)
          case _ => throw new RuntimeException("Unexpected rotation in makeFullImage: " + rotation)
        }
      }

      val maxVoxSize = Math.max(voxSizeX_mm, voxSizeY_mm) // larger voxel dimension
      val minVoxSize = Math.min(voxSizeX_mm, voxSizeY_mm) // smaller voxel dimension
      val widthCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 3) / voxSizeX_mm) * scale //* (maxVoxSize / minVoxSize)
      }
      val heightCircle_pix = {
        ((Config.CBCTBBPenumbra_mm * 3) / voxSizeY_mm) * scale //* (maxVoxSize / minVoxSize)
      }

      // ---------------------------------------------------------------------------------

      /**
       * draw circle around BB
       */
      def drawCircle = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.yellow)

        graphics.drawOval(d2i(bbCenter_pix.getX - (widthCircle_pix / 2)), d2i(bbCenter_pix.getY - (heightCircle_pix / 2)), d2i(widthCircle_pix), d2i(heightCircle_pix))

        val radius = Math.sqrt((widthCircle_pix * widthCircle_pix) + (heightCircle_pix * heightCircle_pix)) / 4
        graphics.drawLine(d2i(bbCenter_pix.getX - radius + 1), d2i(bbCenter_pix.getY - radius), d2i(bbCenter_pix.getX + radius), d2i(bbCenter_pix.getY + radius))
        graphics.drawLine(d2i(bbCenter_pix.getX + radius - 1), d2i(bbCenter_pix.getY - radius), d2i(bbCenter_pix.getX - radius), d2i(bbCenter_pix.getY + radius))
      }

      // ---------------------------------------------------------------------------------

      def drawPlanCenter = {
        val graphics = ImageUtil.getGraphics(image)
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

      def drawPlanText = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.green)
        // show BB offset from plan
        val text = "P"

        val xOffset = if (bbCenter_pix.getX > planCenter_pix.getX) -1 else 1
        val yOffset = if (bbCenter_pix.getY > planCenter_pix.getY) -1 else 1

        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
        val textRect = ImageText.getTextDimensions(graphics, text)
        ImageText.drawTextCenteredAt(graphics, planCenter_pix.getX + xOffset * textPointSize, planCenter_pix.getY + yOffset * textPointSize, text)
      }

      // ---------------------------------------------------------------------------------

      def drawCrossLines = {
        val graphics = ImageUtil.getGraphics(image)
        val dashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(6, 2), 0)
        graphics.setStroke(dashedLine)

        graphics.setColor(vertCenterline)
        graphics.drawLine(d2i(bbCenter_pix.getX), 0, d2i(bbCenter_pix.getX), image.getHeight - 1)

        graphics.setColor(horzCenterline)
        graphics.drawLine(0, d2i(bbCenter_pix.getY), image.getWidth - 1, d2i(bbCenter_pix.getY))
      }

      // ---------------------------------------------------------------------------------

      drawCircle
      drawPlanCenter
      drawPlanText
      drawCrossLines
      image
    }

    // ---------------------------------------------------------------------------------

    def cropImage(image: BufferedImage) = {
      val x = Math.max(0, d2i(bbCenter_pix.getX - (cropSize_pix / 2)))
      val y = Math.max(0, d2i(bbCenter_pix.getY - (cropSize_pix / 2)))

      Trace.trace("x: " + x +
        "    y: " + y +
        "    cropSize_pix: " + cropSize_pix +
        "    image.getWidth: " + image.getWidth +
        "    image.getHeight: " + image.getHeight)
      val aoi = image.getSubimage(x, y, Math.min(cropSize_pix, image.getWidth - x - 1), Math.min(cropSize_pix, image.getHeight - y - 1)) // TODO can get exception
      aoi
    }

    // ---------------------------------------------------------------------------------

    val full = makeFullImage
    val aoi = cropImage(full)

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
   * @param bb_vox BB position in voxels in the the CBCT voxel space.
   *
   * @param rtplanOrigin_vox RTPLAN origin in voxels in the the CBCT voxel space.
   */
  def annotate(bbByCBCT: BBbyCBCT, imageXYZ: Seq[BufferedImage], runReq: BBbyCBCTRunReq, bb_vox: Point3d, rtplanOrigin_vox: Point3d, date: Date): ImageSet = {
    logger.info("Annotating CBCT images for " + bbByCBCT)
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbctList) // the size of a voxel in mm
    logger.info("Annotating CBCT images with voxels sized in mm: " + voxSize_mm.map(s => Util.fmtDbl(s)).mkString(", "))

    val volTrans = new VolumeTranslator(runReq.cbctList)

    val j0 = volTrans.mm2vox(new Point3d(bbByCBCT.cbctX_mm, bbByCBCT.cbctY_mm, bbByCBCT.cbctZ_mm))
    val j1 = volTrans.mm2vox(new Point3d(bbByCBCT.rtplanX_mm, bbByCBCT.rtplanY_mm, bbByCBCT.rtplanZ_mm))
    Trace.trace(j0)
    Trace.trace(j1)
    Trace.trace

    def xImagePair = makePair(
      voxSizeX_mmOrig = voxSize_mm(2), voxSizeY_mmOrig = voxSize_mm(1),
      bb_pix = new Point2d(bb_vox.getZ, bb_vox.getY),
      rtplanOrigin_pix = new Point2d(rtplanOrigin_vox.getZ, rtplanOrigin_vox.getY),
      originalImage = imageXYZ(0),
      Color.green, Color.blue,
      rotation = 90)

    def yImagePair = makePair(
      voxSize_mm(2), voxSize_mm(0),
      bb_pix = new Point2d(bb_vox.getZ, bb_vox.getX),
      rtplanOrigin_pix = new Point2d(rtplanOrigin_vox.getZ, rtplanOrigin_vox.getX),
      imageXYZ(1),
      Color.red, Color.blue,
      rotation = 90)

    def zImagePair = makePair(
      voxSize_mm(0), voxSize_mm(1),
      bb_pix = new Point2d(bb_vox.getX, bb_vox.getY),
      rtplanOrigin_pix = new Point2d(rtplanOrigin_vox.getX, rtplanOrigin_vox.getY),
      imageXYZ(2),
      Color.red, Color.green,
      rotation = 180)

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

    def applyOrientationLegend(image: BufferedImage, label: String, pngFileName: String, pctWidth: Double, top: String, bottom: String, left: String, right: String) = {

      def applyLegend = {
        val file = new File(Config.imageDirFile, pngFileName)
        try {
          val legend = ImageIO.read(file)
          val watermark = new Watermark(legend, false, true, pctWidth, 50.0)
          watermark.mark(image)
        } catch {
          case t: Throwable => logger.warn("Unable to apply watermark with file " + file.getAbsolutePath)
        }
      }

      def label4sides = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.red)
        val pointSize = (textPointSize * 2.5).round.toInt
        ImageText.setFont(graphics, ImageText.DefaultFont, pointSize)

        ImageText.drawTextCenteredAt(graphics, image.getWidth / 2, pointSize, top)
        ImageText.drawTextCenteredAt(graphics, image.getWidth / 2, image.getHeight - pointSize, bottom)
        ImageText.drawTextCenteredAt(graphics, pointSize, image.getHeight / 2, left)
        ImageText.drawTextCenteredAt(graphics, image.getWidth - pointSize, image.getHeight / 2, right)
      }

      def applyLabel = {
        val graphics = ImageUtil.getGraphics(image)
        graphics.setColor(Color.white)
        val pointSize = (textPointSize * 2.5).round.toInt
        ImageText.setFont(graphics, ImageText.DefaultFont, pointSize)
        val dateFormat = new SimpleDateFormat("M/d/yyyy h:mm aa")
        val text = label + " - CBCT - " + dateFormat.format(date)
        graphics.drawString(text, 3, pointSize)
      }

      applyLegend
      label4sides
      applyLabel
    }

    applyOrientationLegend(fullList(0), "Sagittal", "HumanSagittal.png", 5.0, "H", "F", "P", "A")
    applyOrientationLegend(fullList(1), "Frontal", "HumanFrontal.png", 10.0, "H", "F", "R", "L")
    applyOrientationLegend(fullList(2), "Transversal", "HumanTransversal.png", 10.0, "A", "P", "R", "L")

    val imageSet = new ImageSet(
      fullList,
      pairList.map(p => p.areaOfInterest))
    imageSet
  }
}