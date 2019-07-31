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

object BBbyCBCTAnnotateImages {

  /** Factor to magnify image. */
  private val scale = 32

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
  def annotate(bbByCBCT: BBbyCBCT, imageXYZ: Seq[BufferedImage], runReq: BBbyCBCTRunReq, origPosition: Point3d): Seq[BufferedImage] = {
    val voxSize_mm = Util.getVoxSize_mm(runReq.cbct) // the size of a voxel in mm

    val volTrans = new VolumeTranslator(runReq.cbct)

    val zImage = ImageUtil.magnify(imageXYZ(2), scale)

    def rnd(d: Double) = d.round.toInt

    val center_vox = volTrans.mm2vox(origPosition)

    val graphics = ImageUtil.getGraphics(zImage)

    def fmt(d: Double) = d.formatted("%8.3f")

    // ---------------------------------------------------------------------------------

    // draw circle around BB
    graphics.setColor(Color.white)

    val width = ((Config.DailyPhantomBBPenumbra_mm * 0.5) / voxSize_mm(0)) * scale
    val height = ((Config.DailyPhantomBBPenumbra_mm * 0.5) / voxSize_mm(1)) * scale

    val centerX_pix = (center_vox.getX + 0.5) * scale
    val centerY_pix = (center_vox.getY + 0.5) * scale

    graphics.drawOval(rnd(centerX_pix - (width / 2)), rnd(centerY_pix - (height / 2)), rnd(width), rnd(height))

    val radius = Math.sqrt((width * width) + (height * height)) / 4
    graphics.drawLine(rnd(centerX_pix - radius), rnd(centerY_pix - radius), rnd(centerX_pix + radius), rnd(centerY_pix + radius))
    graphics.drawLine(rnd(centerX_pix + radius), rnd(centerY_pix - radius), rnd(centerX_pix - radius), rnd(centerY_pix + radius))

    // crop image
    val xRadius = (Config.DailyPhantomSearchDistance_mm / voxSize_mm(0)) * scale
    val yRadius = (Config.DailyPhantomSearchDistance_mm / voxSize_mm(1)) * scale
    val aoi = zImage.getSubimage(rnd(centerX_pix - xRadius), rnd(centerY_pix - yRadius), rnd(xRadius * 2), rnd(yRadius * 2))

    // ---------------------------------------------------------------------------------

    // show BB offset from plan
    val bbText = "Offset: " +
      fmt(bbByCBCT.rtplanX_mm - bbByCBCT.cbctX_mm) + ", " +
      fmt(bbByCBCT.rtplanY_mm - bbByCBCT.cbctY_mm) + ", " +
      fmt(bbByCBCT.rtplanZ_mm - bbByCBCT.cbctZ_mm)

    ImageText.setFont(graphics, ImageText.DefaultFont, 45)
    ImageText.drawTextOffsetFrom(graphics, centerX_pix, centerY_pix - (height / 2) + 10, bbText, 90)

    // ---------------------------------------------------------------------------------

    graphics.setColor(Color.red)

    val invMatrix = {
      val m = runReq.reg.getMatrix.clone.asInstanceOf[Matrix4d]
      m.invert
      m
    }

    val planInCBCTFrame = {
      val p = new Point3d(bbByCBCT.rtplanX_mm, bbByCBCT.rtplanY_mm, bbByCBCT.rtplanX_mm)
      invMatrix.transform(p)
      p
    }

    val plan_vox = volTrans.mm2vox(planInCBCTFrame)

    val plan_vox_scaled = new Point3d(plan_vox.getX * scale, plan_vox.getY * scale, plan_vox.getZ * scale)
    graphics.drawLine(rnd(plan_vox_scaled.getX), rnd(plan_vox_scaled.getY - scale), rnd(plan_vox_scaled.getX), rnd(plan_vox_scaled.getY + scale))
    graphics.drawLine(rnd(plan_vox_scaled.getX - scale), rnd(plan_vox_scaled.getY), rnd(plan_vox_scaled.getX + scale), rnd(plan_vox_scaled.getY))

    // ---------------------------------------------------------------------------------

    def ff(d: Double) = d.formatted("%10.5f")
    Trace.trace("rtplan: " + ff(bbByCBCT.rtplanX_mm) + "  " + ff(bbByCBCT.rtplanY_mm) + "  " + ff(bbByCBCT.rtplanZ_mm))
    Trace.trace("cbct  : " + ff(bbByCBCT.cbctX_mm) + "  " + ff(bbByCBCT.cbctY_mm) + "  " + ff(bbByCBCT.cbctZ_mm))
    Trace.trace("offset: " + ff(bbByCBCT.rtplanX_mm - bbByCBCT.cbctX_mm) + "  " + ff(bbByCBCT.rtplanY_mm - bbByCBCT.cbctY_mm) + "  " + ff(bbByCBCT.rtplanZ_mm - bbByCBCT.cbctZ_mm))

    // ---------------------------------------------------------------------------------

    if (true) { // TODO rm
      val testDir = new File("""D:\tmp\aqa\tmp""")
      testDir.mkdirs
      val zFile = new File(testDir, "z.png")
      zFile.delete
      Thread.sleep(100)
      ImageUtil.writePngFile(aoi, zFile)
      Trace.trace("wrote file " + zFile.getAbsolutePath)
    }

    Seq[BufferedImage]() // TODO rm
  }
}