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

object BBbyCBCTAnnotateImages {

  private val imageScaleFactor = 10

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

    val zImage = ImageUtil.magnify(imageXYZ(2), imageScaleFactor)
    def rnd(d: Double) = d.round.toInt

    val center_vox = volTrans.mm2vox(origPosition)

    def drawCircleAroundBB = {
      val graphics = zImage.getGraphics
      graphics.setColor(Color.white)
      val width = ((Config.DailyPhantomBBPenumbra_mm * 0.75) / voxSize_mm(0)) * imageScaleFactor
      val height = ((Config.DailyPhantomBBPenumbra_mm * 0.75) / voxSize_mm(1)) * imageScaleFactor
      val centerX = ((origPosition.getX / voxSize_mm(0)) - 0.5) * imageScaleFactor
      val centerY = ((origPosition.getY / voxSize_mm(1)) - 0.5) * imageScaleFactor
      val x = (center_vox.getX * imageScaleFactor) - (width / 2)
      val y = (center_vox.getY * imageScaleFactor) - (height / 2)
      graphics.drawOval(rnd(x), rnd(y), rnd(width), rnd(height))
    }

    drawCircleAroundBB

    if (true) { // TODO rm
      val testDir = new File("""D:\tmp\aqa\tmp""")
      testDir.mkdirs
      val zFile = new File(testDir, "z.png")
      zFile.delete
      Thread.sleep(100)
      ImageUtil.writePngFile(zImage, zFile)
    }

    Seq[BufferedImage]() // TODO rm
  }
}