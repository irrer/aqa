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
  private val scaleAoi = 16

  /** Factor to magnify full sized image. */
  private val scaleFullSized = 8

  private def fmt(d: Double) = d.formatted("%8.3f")

  case class ImageSet(fullImage: Seq[BufferedImage], areaOfInterest: Seq[BufferedImage]);

  private case class ImagePair(fullImage: BufferedImage, areaOfInterest: BufferedImage);

  private def zMakePair(bbByCBCT: BBbyCBCT, voxSize_mm: Seq[Double], volTrans: VolumeTranslator, originalImage: BufferedImage, origPosition: Point3d): ImagePair = {

    def rnd(d: Double) = d.round.toInt

    val center_vox = volTrans.mm2vox(origPosition)

    // ---------------------------------------------------------------------------------
    val centerX_pix = (center_vox.getX + 0.5) * scaleAoi
    val centerY_pix = (center_vox.getY + 0.5) * scaleAoi

    def makeImage = {
      val zImage = ImageUtil.magnify(originalImage, scaleAoi)

      val width = ((Config.DailyPhantomBBPenumbra_mm * 0.5) / voxSize_mm(0)) * scaleAoi
      val height = ((Config.DailyPhantomBBPenumbra_mm * 0.5) / voxSize_mm(1)) * scaleAoi

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
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.white)
        // show BB offset from plan
        val bbText = "Offset: " +
          fmt(bbByCBCT.rtplanX_mm - bbByCBCT.cbctX_mm) + ", " +
          fmt(bbByCBCT.rtplanY_mm - bbByCBCT.cbctY_mm) + ", " +
          fmt(bbByCBCT.rtplanZ_mm - bbByCBCT.cbctZ_mm)

        ImageText.setFont(graphics, ImageText.DefaultFont, 30)
        ImageText.drawTextOffsetFrom(graphics, centerX_pix, centerY_pix - (height / 2) + 10, bbText, 90)
      }

      // ---------------------------------------------------------------------------------

      def drawPlanCenter(zImage: BufferedImage) = {
        val graphics = ImageUtil.getGraphics(zImage)
        graphics.setColor(Color.red)

        val planXcenter = {
          val diff = (bbByCBCT.cbctX_mm - bbByCBCT.rtplanX_mm) / voxSize_mm(0)
          centerX_pix + (diff * scaleAoi)
        }

        val planYcenter = {
          val diff = (bbByCBCT.cbctY_mm - bbByCBCT.rtplanY_mm) / voxSize_mm(1)
          centerY_pix + (diff * scaleAoi)
        }

        graphics.drawLine(
          rnd(planXcenter), rnd(planYcenter - scaleAoi),
          rnd(planXcenter), rnd(planYcenter + scaleAoi))
        graphics.drawLine(
          rnd(planXcenter - scaleAoi), rnd(planYcenter),
          rnd(planXcenter + scaleAoi), rnd(planYcenter))
      }

      drawCircle(zImage)
      drawOffsetNumbers(zImage)
      drawPlanCenter(zImage)
      zImage
    }

    def cropImage(image: BufferedImage) = {
      val xRadius = ((Config.DailyPhantomSearchDistance_mm / 3.0) / voxSize_mm(0)) * scaleAoi
      val yRadius = ((Config.DailyPhantomSearchDistance_mm / 3.0) / voxSize_mm(1)) * scaleAoi

      val aoi = image.getSubimage(rnd(centerX_pix - xRadius), rnd(centerY_pix - yRadius), rnd(xRadius * 2), rnd(yRadius * 2))
      aoi
    }

    val full = makeImage
    val aoi = cropImage(full)

    Util.addAxisLabels(aoi, "X Axis", "Y Axis", Color.white)

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
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbct) // the size of a voxel in mm

    val volTrans = new VolumeTranslator(runReq.cbct)

    val zImagePair = zMakePair(bbByCBCT, voxSize_mm, volTrans, imageXYZ(2), origPosition)

    val imageSet = new ImageSet(
      Seq(zImagePair.fullImage, zImagePair.fullImage, zImagePair.fullImage),
      Seq(zImagePair.areaOfInterest, zImagePair.areaOfInterest, zImagePair.areaOfInterest))
    imageSet
  }
}