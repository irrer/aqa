package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ScaledImage
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.Stakitt
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries
import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Util

import java.awt.Color
import java.awt.image.BufferedImage

case class LeafEndPositions(extendedData: ExtendedData, dicomImage: DicomImage, xImageBorders: Seq[Double], yImageBorders: LeafBoundaries, rtimage: AttributeList) extends Logging {

  private val trans = new IsoImagePlaneTranslator(rtimage)

  /** Scale of image created.  For drawing purposes only. */
  private val scale = 7

  private val oneMM_pix: Double = trans.iso2PixDistX(1)

  /** Buffered image for display to the user. */
  private val bufImg: BufferedImage = {
    val img = dicomImage.toDeepColorBufferedImage(0.01)
    ImageUtil.magnify(img, scale)
  }

  /** Show image origin. */
  Config.applyWatermark(bufImg)

  private val gc = ImageUtil.getGraphics(bufImg)
  gc.setColor(Color.white)

  private val si = ScaledImage(scale, 0, 0)

  private def drawSinglePixelOfLeafEdge(x_pix: Double, y_pix: Int): Unit = {
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
    * @param stakittAOI Containing AOI.
    */
  private def annotateLeafEnd(xPosition_pix: Double, stakittAOI: StakittAOI): Unit = {
    val text = "%10.3f".format(trans.pix2IsoCoordX(xPosition_pix)).trim
    val textX = xPosition_pix + (ImageText.getTextDimensions(gc, text).getWidth * 0.8) / scale
    val textY = stakittAOI.rectangle.y + (stakittAOI.rectangle.height / 2)

    val textDimensions = ImageText.getTextDimensions(gc, text)
    val expansionHorizontal_pix = 4
    val expansionVertical_pix = 2

    def rnd(d: Double): Int = d.round.toInt

    val backgroundColor = new Color(0xb1d1fc)

    gc.setColor(backgroundColor)
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
  private def annotateAOIEdge(stakittAOI: StakittAOI, top: Boolean = true): Unit = {

    val y_pix = if (top) stakittAOI.rectangle.y else stakittAOI.rectangle.y + stakittAOI.rectangle.height
    val xCenter_pix = stakittAOI.rectangle.x + (stakittAOI.rectangle.width / 2)

    val x1a = xCenter_pix - (3 * oneMM_pix)
    val x2a = xCenter_pix - oneMM_pix

    val x1b = xCenter_pix + oneMM_pix
    val x2b = xCenter_pix + (3 * oneMM_pix)

    gc.setColor(Color.black)
    ImageUtil.setLineThickness(gc, 1.0)
    si.drawLine(gc, x1a, y_pix, x2a, y_pix)
    si.drawLine(gc, x1b, y_pix, x2b, y_pix)
  }

  /**
    * Measures the position of the leaf's end in absolute (not relative) pixels.
    * @param stakittAOI Area of interest for one leaf. This includes a margin that separates vertical consecutive AOIs.
    * @return End of leaf in absolute pixels.
    */
  private def measureLeafEnd(stakittAOI: StakittAOI, leafEndBySinglePixel: LeafEndBySinglePixel): Double = {

    val rect = stakittAOI.rectangle //  makeAOIWithMargin_pix(xIndex, yIndex)

    // Edge position for each individual row of pixels in the leaf.
    val pixelWiseEdgeList_pix = stakittAOI.yCoordinateList.map(y_pix => leafEndBySinglePixel.get(stakittAOI.rectangle.x, y_pix))

    val xPosition_pix = {
      // the sum of all the edge positions of the individual rows of pixels, with the top and bottom rows weighted in proportion
      // to their contribution of the edge.  So for example if only 30% a row of pixels is in the AOI, then multiply that row's
      // edge by 0.30
      val xPosition_sum = {
        val list = pixelWiseEdgeList_pix.zip(stakittAOI.yWeightList).map(vw => vw._1 * vw._2)
        list.sum
      }

      // divide the sum of positions by the total height of the rectangle to get the mean position of the leaf end
      xPosition_sum / rect.height
    }

    gc.setColor(Color.black)
    ImageUtil.setLineThickness(gc, 3.0)
    si.drawLine(gc, xPosition_pix, rect.y, xPosition_pix, rect.y + rect.height)

    annotateLeafEnd(xPosition_pix, stakittAOI)

    xPosition_pix
  }

  private def constructStakittResult(stakittAOI: StakittAOI, leafEndBySinglePixel: LeafEndBySinglePixel): StakittResult = {
    val edgePosition_pix = measureLeafEnd(stakittAOI, leafEndBySinglePixel)
    val edgePosition_mm = trans.pix2IsoCoordX(edgePosition_pix)

    // gc.setColor(Color.gray)
    // si.drawRect(gc, stakittAOI.rectangle)

    val stakitt = Stakitt( //
      stakittPK = None,
      outputPK = extendedData.outputPK,
      SOPInstanceUID = Util.sopOfAl(rtimage),
      beamName = "NA", // TODO
      leafIndex = stakittAOI.yIndex + 1,
      leafPositionIndex = stakittAOI.xIndex + 1,
      measuredEndPosition_mm = edgePosition_mm,
      plannedEndPosition_mm = -1, // TODO
      measuredMinorSide_mm = trans.pix2IsoCoordY(stakittAOI.rectangle.y),
      measuredMajorSide_mm = trans.pix2IsoCoordY(stakittAOI.rectangle.y + stakittAOI.rectangle.height)
    )

    val result = StakittResult(stakitt, stakittAOI)
    result
  }

  /**
    * Draw a line that show the top and bottom bounds of the leaf boundaries.
    * @param aoiList List of all AOIs.
    */
  private def annotateAoiTopAndBottomBounds(aoiList: Seq[StakittAOI]): Unit = {
    aoiList.foreach(aoi => annotateAOIEdge(aoi))
    val maxYIndex = aoiList.map(_.yIndex).max
    aoiList.filter(_.yIndex == maxYIndex).foreach(aoi => annotateAOIEdge(aoi, top = false))
  }

  def measureLeafPositions(): Seq[StakittResult] = {

    // val j = (0 until xAoiPairList.size).map(doColumn)

    val aoiList = StakittAOI.makeAOIs(xImageBorders, yImageBorders, rtimage)

    // val xAoiBorders = XAoiBorders.makeXPairList(xImageBorders.xPointList)

    val xAoiPairList = XAoiBorders.makeXPairList(xImageBorders)
    val leafEndBySinglePixel = LeafEndBySinglePixel(xAoiPairList, yImageBorders, trans, dicomImage)
    aoiList.map(aoi => constructStakittResult(aoi, leafEndBySinglePixel))

    xAoiPairList.foreach(xAoi =>
      (leafEndBySinglePixel.minY until leafEndBySinglePixel.maxY).foreach(y_pix => {
        drawSinglePixelOfLeafEdge(leafEndBySinglePixel.get(xAoi.lo, y_pix), y_pix)
      })
    )

    annotateAoiTopAndBottomBounds(aoiList)

    Trace.showInMSPaint(bufImg) // TODO rm

    Seq()

  }

}
