package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import edu.umro.ScalaUtil.Trace
import java.awt.image.BufferedImage
import org.aqa.db.LeafPosition
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import org.aqa.Config
import edu.umro.ImageUtil.ImageText
import org.aqa.Util
import java.awt.BasicStroke

object LeafPositionAnalysisAnnotateImage extends Logging {

  /** Zoom factor to use when enlarging image. */
  private val zoom = 4

  private val dashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(3, 6), 0)

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

    def i2pX(x: Double) = (translator.iso2PixCoordX(x) * zoom).round.toInt
    def i2pY(y: Double) = (translator.iso2PixCoordY(y) * zoom).round.toInt

    def annotateExpectedPositions = {
      graphics.setColor(Color.black)
      graphics.setStroke(dashedLine)

      val expectedList = leafPositionList.map(lp => lp.expectedEndPosition_mm).distinct.sorted
      val minEnd = expectedList.head
      val maxEnd = expectedList.last
      val minSide = leafPositionList.map(_.measuredMinorSide_mm).min
      val maxSide = leafPositionList.map(_.measuredMajorSide_mm).max

      def drawExpected(exp_mm: Double) = {
        val x1 = if (horizontal) exp_mm else minSide
        val y1 = if (horizontal) minSide else exp_mm

        val x2 = if (horizontal) exp_mm else maxSide
        val y2 = if (horizontal) maxSide else exp_mm
        graphics.drawLine(i2pX(x1), i2pY(y1), i2pX(x2), i2pY(y2))

        val text = "Plan:  " + Util.fmtDbl(exp_mm)
        val degreesMinor = if (horizontal) 90 else 180
        ImageText.drawTextOffsetFrom(graphics, i2pX(x1), i2pY(y1), text, degreesMinor)
        val degreesMajor = (degreesMinor + 180) % 360
        ImageText.drawTextOffsetFrom(graphics, i2pX(x2), i2pY(y2), text, degreesMajor)
      }

      expectedList.map(exp_mm => drawExpected(exp_mm))
    }

    def annotateEnds = {
      graphics.setColor(Color.black)
      ImageUtil.setSolidLine(graphics)

      val isol = Config.LeafPositionIsolationDistance_mm
      def annotateLeafPosition(lp: LeafPosition) = {
        val x1 = if (horizontal) lp.measuredEndPosition_mm else lp.measuredMinorSide_mm + isol
        val y1 = if (horizontal) lp.measuredMinorSide_mm + isol else lp.measuredEndPosition_mm

        val x2 = if (horizontal) lp.measuredEndPosition_mm else lp.measuredMajorSide_mm - isol
        val y2 = if (horizontal) lp.measuredMajorSide_mm - isol else lp.measuredEndPosition_mm

        graphics.drawLine(i2pX(x1), i2pY(y1), i2pX(x2), i2pY(y2))

        val degrees = (horizontal, lp.offset_mm < 0) match {
          case (true, true) => 180
          case (true, false) => 0
          case (false, true) => 90
          case (false, false) => 270
        }

        val text = lp.offset_mm.formatted("%7.4f")
        ImageText.drawTextOffsetFrom(graphics, i2pX((x1 + x2) / 2), i2pY((y1 + y2) / 2), text, degrees)
      }
      leafPositionList.map(lp => annotateLeafPosition(lp))
    }

    annotateExpectedPositions // put this on the bottom layer, may be overwritten by measured positions
    annotateEnds

    bufImg
  }

}