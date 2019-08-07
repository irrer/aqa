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
  private val scale = 32

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
    voxSizeX_mm: Double, voxSizeY_mm: Double,
    centerX_pix: Double, centerY_pix: Double,
    xAxisName: String, yAxisName: String,
    originalImage: BufferedImage): ImagePair = {

    def rnd(d: Double) = d.round.toInt

    // ---------------------------------------------------------------------------------

    def makeFullImage = {
      val image = ImageUtil.magnify(originalImage, scale)

      val maxVoxSize = Math.max(voxSizeX_mm, voxSizeY_mm) // larger voxel dimension
      val minVoxSize = Math.min(voxSizeX_mm, voxSizeY_mm) // smaller voxel dimension
      val width = ((Config.DailyPhantomBBPenumbra_mm * 0.5) / voxSizeX_mm) * scale * (maxVoxSize / minVoxSize)
      val height = ((Config.DailyPhantomBBPenumbra_mm * 0.5) / voxSizeY_mm) * scale

      /**
       * draw circle around BB
       */
      def drawCircle(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.white)

        graphics.drawOval(rnd(centerX_pix - (width / 2)), rnd(centerY_pix - (height / 2)), rnd(width), rnd(height))

        val radius = Math.sqrt((width * width) + (height * height)) / 4
        graphics.drawLine(rnd(centerX_pix - radius + 1), rnd(centerY_pix - radius), rnd(centerX_pix + radius), rnd(centerY_pix + radius))
        graphics.drawLine(rnd(centerX_pix + radius - 1), rnd(centerY_pix - radius), rnd(centerX_pix - radius), rnd(centerY_pix + radius))
      }

      // ---------------------------------------------------------------------------------

      def drawOffsetNumbers(zImage: BufferedImage) = {
        def fmt(d: Double) = d.formatted("%8.2f")
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.white)
        // show BB offset from plan
        val bbText = (
          "Offset: " +
          fmt(bbByCBCT.rtplanX_mm - bbByCBCT.cbctX_mm) + ", " +
          fmt(bbByCBCT.rtplanY_mm - bbByCBCT.cbctY_mm) + ", " +
          fmt(bbByCBCT.rtplanZ_mm - bbByCBCT.cbctZ_mm)).replaceAll("  *", " ")

        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
        ImageText.drawTextOffsetFrom(graphics, centerX_pix, centerY_pix - (height / 2) + 10, bbText, 90)
      }

      // ---------------------------------------------------------------------------------

      def drawPlanCenter(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.red)

        val planXcenter = {
          val diff = (bbByCBCT.cbctZ_mm - bbByCBCT.rtplanZ_mm) / voxSizeX_mm
          centerX_pix + (diff * scale)
        }

        val planYcenter = {
          val diff = (bbByCBCT.cbctY_mm - bbByCBCT.rtplanY_mm) / voxSizeY_mm
          centerY_pix + (diff * scale)
        }

        graphics.drawLine(
          rnd(planXcenter), rnd(planYcenter - scale),
          rnd(planXcenter), rnd(planYcenter + scale))
        graphics.drawLine(
          rnd(planXcenter - scale), rnd(planYcenter),
          rnd(planXcenter + scale), rnd(planYcenter))
      }

      // ---------------------------------------------------------------------------------

      def drawPlanText(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.red)
        // show BB offset from plan
        val text = "'+' is Plan Center"

        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
        ImageText.drawTextOffsetFrom(graphics, centerX_pix, centerY_pix + (height / 2) + scale, text, 90)
      }

      drawCircle(image)
      drawOffsetNumbers(image)
      drawPlanCenter(image)
      drawPlanText(image)
      image
    }

    // ---------------------------------------------------------------------------------

    def cropImage(image: BufferedImage) = {
      val xRadius = ((Config.DailyPhantomSearchDistance_mm / 3.0) / voxSizeX_mm) * scale
      val yRadius = ((Config.DailyPhantomSearchDistance_mm / 3.0) / voxSizeY_mm) * scale

      val x = Math.max(0, rnd(centerX_pix - xRadius))
      val y = Math.max(0, rnd(centerY_pix - yRadius))
      val width = {
        val w = rnd(xRadius * 2)
        if ((w + x) > image.getWidth) image.getWidth else w
      }
      val height = {
        val h = rnd(yRadius * 2)
        if ((h + y) > image.getHeight) image.getHeight else h
      }
      val aoi = image.getSubimage(x, y, width, height)
      aoi
    }

    // ---------------------------------------------------------------------------------

    val full = makeFullImage
    val aoi = cropImage(full)

    Util.addAxisLabels(aoi, xAxisName, yAxisName, Color.white)

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
    Trace.trace
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbct) // the size of a voxel in mm
    Trace.trace

    val volTrans = new VolumeTranslator(runReq.cbct)
    Trace.trace

    val center_vox = volTrans.mm2vox(origPosition)
    val centerX_pix = (center_vox.getX + 0.5) * scale
    val centerY_pix = (center_vox.getY + 0.5) * scale
    val centerZ_pix = (center_vox.getZ + 0.5) * scale
    Trace.trace

    val xImagePair = makePair(
      bbByCBCT,
      voxSize_mm(2), voxSize_mm(1),
      centerZ_pix, centerY_pix,
      "Z axis", "Y axis", imageXYZ(0))
    Trace.trace

    val yImagePair = makePair(
      bbByCBCT,
      voxSize_mm(2), voxSize_mm(0),
      centerZ_pix, centerX_pix,
      "Z axis", "X axis", imageXYZ(1))
    Trace.trace

    val zImagePair = makePair(
      bbByCBCT,
      voxSize_mm(0), voxSize_mm(1),
      centerX_pix, centerY_pix,
      "X axis", "Y axis", imageXYZ(2))

    Trace.trace

    val imageSet = new ImageSet(
      Seq(xImagePair.fullImage, yImagePair.fullImage, zImagePair.fullImage),
      Seq(xImagePair.areaOfInterest, yImagePair.areaOfInterest, zImagePair.areaOfInterest))
    Trace.trace
    imageSet
  }
}