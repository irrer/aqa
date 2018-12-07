package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import edu.umro.ScalaUtil.Trace
import java.awt.image.BufferedImage
import org.aqa.db.LeafPosition
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color

object LeafPositionAnalysisAnnotateImage extends Logging {

  /** Zoom factor to use when enlarging image. */
  private val zoom = 10

  private def makeZoomedImage(dicomImage: DicomImage): BufferedImage = {
    val bufImg = new BufferedImage(dicomImage.width * zoom, dicomImage.height * zoom, BufferedImage.TYPE_INT_RGB)

    val originalImage = dicomImage.toDeepColorBufferedImage

    for (x <- 0 until dicomImage.width; y <- 0 until dicomImage.height) {
      val rgb = originalImage.getRGB(x, y)

      for (xx <- 0 until zoom; yy <- 0 until zoom) bufImg.setRGB(x * zoom + xx, y * zoom + yy, rgb)
    }
    bufImg
  }

  /**
   * Create an image that has the leaf positions annotated.
   */
  def annotateImage(leafPositionList: Seq[LeafPosition], horizontal: Boolean, dicomImage: DicomImage, leafWidthList_mm: Seq[Double], translator: IsoImagePlaneTranslator): BufferedImage = {
    val bufImg = makeZoomedImage(dicomImage)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(Color.black)

    def i2pX(x: Double) = (translator.iso2PixCoordX(x) * zoom).round.toInt
    def i2pY(y: Double) = (translator.iso2PixCoordY(y) * zoom).round.toInt

    def annotateLeafPosition(lp: LeafPosition) = {
      val x1 = if (horizontal) lp.measuredEndPosition_mm else lp.measuredMinorSide_mm
      val y1 = if (horizontal) lp.measuredMinorSide_mm else lp.measuredEndPosition_mm

      val x2 = if (horizontal) lp.measuredEndPosition_mm else lp.measuredMajorSide_mm
      val y2 = if (horizontal) lp.measuredMajorSide_mm else lp.measuredEndPosition_mm

      graphics.drawLine(i2pX(x1), i2pY(y1), i2pX(x2), i2pY(y2))
    }

    leafPositionList.map(lp => annotateLeafPosition(lp))
    bufImg
  }

}