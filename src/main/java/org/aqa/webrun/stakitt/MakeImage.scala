package org.aqa.webrun.stakitt

import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ScaledImage
import org.aqa.Config
import org.aqa.Logging

import java.awt.Color
import java.awt.image.BufferedImage

object MakeImage extends Logging {

  /** Scale of image created.  For drawing purposes only. */
  val scale: Int = 2

  /**
   * Make an annotated image showing the leaf ends.
   * @param analysis Data from analysis.
   * @return An image.
   */
  def makeImage(analysis: Analysis): BufferedImage = {

    val trans = new IsoImagePlaneTranslator(analysis.rtimage)

    val oneMM_pix: Double = trans.iso2PixDistX(1)

    /** Buffered image for display to the user. */
    val bufImg: BufferedImage = {
      val img = analysis.dicomImage.toDeepColorBufferedImage(0.01)
      ImageUtil.magnify(img, scale)
    }

    /** Show image origin. */
    Config.applyWatermark(bufImg)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    val si = ScaledImage(scale, 0, 0)

    def drawSinglePixelOfLeafEdge(x_pix: Double, y_pix: Int): Unit = {
      val x = si.scalePixelX(x_pix)
      //  val y1 = (index * si.scale) + si.scalePixelY(stakittAOI.rectangle.y) - (si.scale / 2) - 1
      val y1 = si.scalePixelY(y_pix) - (scale / 2)
      val y2 = (y1 + si.scale) - 1

      gc.setColor(Color.white)
      ImageUtil.setLineThickness(gc, 1.0)
      gc.drawLine(x, y1, x, y2)
    }

    /**
      * Annotate a leaf end with it's measured value.  Set the background color of the text to have good contrast.
      * @param xPosition_pix Leaf position.
      * @param stakittResult Final result and AOI.
      */
    def annotateLeafEnd(xPosition_pix: Double, stakittResult: StakittResult): Unit = {

      val rect = stakittResult.stakittAOI.rectangle

      val text = "%10.3f".format(stakittResult.stakitt.leafEndOffset_mm).trim
      val textX = xPosition_pix + (ImageText.getTextDimensions(gc, text).getWidth * 0.8) / scale
      val textY = rect.y + (rect.height / 2)

      val textDimensions = ImageText.getTextDimensions(gc, text)
      val expansionHorizontal_pix = 4
      val expansionVertical_pix = 2

      def rnd(d: Double): Int = d.round.toInt

      gc.setColor(Color.lightGray)
      gc.fillRect(
        rnd(si.scalePixelX(textX) - (textDimensions.getWidth / 2) - expansionHorizontal_pix),
        rnd(si.scalePixelY(textY) - (textDimensions.getHeight / 2) - expansionVertical_pix),
        rnd(textDimensions.getWidth + (expansionHorizontal_pix * 2)),
        rnd(textDimensions.getHeight + (expansionVertical_pix * 2))
      )

      gc.setColor(Color.darkGray)
      si.drawTextCenteredAt(gc, textX, textY, text)
    }

    /**
      * Annotate a leaf end with it's measured value.  Set the background color of the text to have good contrast.
      * @param stakittAOI Containing AOI.
      */
    def annotateAOIEdge(stakittAOI: StakittAOI, top: Boolean = true): Unit = {

      val rect = stakittAOI.rectangle
      val y_pix = if (top) rect.y else rect.y + rect.height
      val xCenter_pix = rect.x + (rect.width / 2)

      val x1a = rect.x
      val x2a = xCenter_pix - oneMM_pix

      val x1b = xCenter_pix + oneMM_pix
      val x2b = rect.x + rect.width

      gc.setColor(Color.black)
      ImageUtil.setLineThickness(gc, 1.0)
      si.drawLine(gc, x1a, y_pix, x2a, y_pix)
      si.drawLine(gc, x1b, y_pix, x2b, y_pix)
    }

    /**
      * Draw the thicker black line denoting the end of the leaf, and annotate it with the error in mm deviation from the plan.
      * @param stakittResult Contains final result for one leaf and AOI used.
      */
    def drawLeafEnd(stakittResult: StakittResult): Unit = {
      val rect = stakittResult.stakittAOI.rectangle
      val xPosition_pix = trans.iso2PixCoordX(stakittResult.stakitt.measuredEndPosition_mm)
      gc.setColor(Color.black)
      ImageUtil.setLineThickness(gc, 3.0)
      si.drawLine(gc, xPosition_pix, rect.y, xPosition_pix, rect.y + rect.height)
      annotateLeafEnd(xPosition_pix, stakittResult)
    }

    /**
      * Draw a line that show the top and bottom bounds of the leaf boundaries.
      * @param aoiList List of all AOIs.
      */
    def annotateAoiTopAndBottomBounds(aoiList: Seq[StakittAOI]): Unit = {
      aoiList.foreach(aoi => annotateAOIEdge(aoi))
      val maxYIndex = aoiList.map(_.yIndex).max
      aoiList.filter(_.yIndex == maxYIndex).foreach(aoi => annotateAOIEdge(aoi, top = false))
    }

    def annotateLeafPositions(): Unit = {
      analysis.xAoiBorders.foreach(xAoi =>
        (analysis.leafEndBySinglePixel.minY until analysis.leafEndBySinglePixel.maxY).foreach(y_pix => {
          drawSinglePixelOfLeafEdge(analysis.leafEndBySinglePixel.get(xAoi.lo, y_pix), y_pix)
        })
      )
      annotateAoiTopAndBottomBounds(analysis.stakittList.map(_.stakittAOI))
    }

    def drawVerticalAoiBorders(): Unit = {
      val list = analysis.xAoiBorders.map(_.lo) :+ analysis.xAoiBorders.last.hi
      gc.setColor(Color.black)
      ImageUtil.setLineThickness(gc, 1.0)
      list.foreach(x_pix => {
        val minY_pix = 5
        val maxY_pix = 5
        si.drawLine(gc, x_pix, minY_pix, x_pix, maxY_pix)
      })
    }

    analysis.stakittList.foreach(drawLeafEnd)

    annotateLeafPositions()

    bufImg

  }
}
