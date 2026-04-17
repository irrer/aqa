package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
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

case class LeafEndPositions(extendedData: ExtendedData, dicomImage: DicomImage, xImageBorders: LeafEnds, yImageBorders: LeafBoundaries, rtimage: AttributeList) extends Logging {

  private val trans = new IsoImagePlaneTranslator(rtimage)

  private def mean(d1: Double, d2: Double) = (d1 + d2) / 2

  private val scale = 5
  private val bufImg: BufferedImage = {
    val img = dicomImage.toDeepColorBufferedImage(0.01)
    ImageUtil.magnify(img, scale)
  }

  Config.applyWatermark(bufImg)

  private val gc = ImageUtil.getGraphics(bufImg)
  gc.setColor(Color.white)

  private val si = ScaledImage(scale, 0, 0)

  private def findRowEdge(y: Int, xRange: Range): Double = {

    val minMaxSampleSize_pix = trans.iso2PixDistX(Config.StakittHorizontalMinMaxSampleLength_mm).round.toInt

    val list = xRange.map(x => dicomImage.get(x, y)) // one single row of pixes in the AOI
    val sorted = list.sorted
    val min = sorted.take(minMaxSampleSize_pix).sum / minMaxSampleSize_pix
    val max = sorted.takeRight(minMaxSampleSize_pix).sum / minMaxSampleSize_pix
    val midValue = mean(min, max)

    val e = LocateEdge.locateEdge(list, midValue)

    val xPosition_pix = xRange.head + e

    xPosition_pix
  }

  private def drawSinglePixelOfLeafEdge(index: Int, xEdgePosition_pix: Double, stakittAOI: StakittAOI): Unit = {
    val x = si.scalePixelX(xEdgePosition_pix)
    //  val y1 = (index * si.scale) + si.scalePixelY(stakittAOI.rectangle.y) - (si.scale / 2) - 1
    val y1 = stakittAOI.yCoordinateList(index) * si.scale
    val y2 = (y1 + si.scale) - 1

    val top = si.scalePixelY(stakittAOI.rectangle.y) + 1
    val bottom = si.scalePixelY(stakittAOI.rectangle.y + stakittAOI.rectangle.height)

    // if (Math.clamp(y1, top, bottom) == y1 || Math.clamp(y2, top, bottom) == y2) {

    if ((y2 >= top) && (y1 <= bottom)) {
      val y1Bounded = Math.clamp(y1, top, bottom)
      val y2Bounded = Math.clamp(y2, top, bottom)

      gc.setColor(Color.white)
      ImageUtil.setLineThickness(gc, 1.0)
      gc.drawLine(x, y1Bounded, x, y2Bounded)
    }
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

    gc.setColor(Color.yellow)
    gc.fillRect(
      rnd(si.scalePixelX(textX) - (textDimensions.getWidth / 2) - expansionHorizontal_pix),
      rnd(si.scalePixelY(textY) - (textDimensions.getHeight / 2) - expansionVertical_pix),
      rnd(textDimensions.getWidth + (expansionHorizontal_pix * 2)),
      rnd(textDimensions.getHeight + (expansionVertical_pix * 2))
    )

    gc.setColor(Color.black)
    si.drawTextCenteredAt(gc, textX, textY, text)
  }

  /**
    * Measures the position of the leaf's end in absolute (not relative) pixels.
    * @param stakittAOI Area of interest for one leaf. This includes a margin that separates vertical consecutive AOIs.
    * @return End of leaf in absolute pixels.
    */
  private def measureLeafEnd(stakittAOI: StakittAOI): Double = {

    val rect = stakittAOI.rectangle //  makeAOIWithMargin_pix(xIndex, yIndex)

    val xRange: Range = rect.x.floor.toInt until (rect.x + rect.width).ceil.toInt

    // Edge position for each individual row of pixels in the leaf.
    val pixelWiseEdgeList_pix = stakittAOI.yCoordinateList.map(y => findRowEdge(y, xRange))

    val xPosition_pix = {

      // the sum of all the edge positions of the individual rows of pixels, with the top and bottom rows weighted in proportion
      // to their contribution of the edge.  So for example if only 30% a row of pixels is in the AOI, then multiply that row's
      // edge by 0.30
      val xPosition_sum = {
        val list = pixelWiseEdgeList_pix.indices.map(i => pixelWiseEdgeList_pix(i) * stakittAOI.yWeightList(i))
        list.sum
      }

      // divide the sum of positions by the total height of the rectangle to get the mean position of the leaf end
      xPosition_sum / rect.height
    }

    gc.setColor(Color.black)
    ImageUtil.setLineThickness(gc, 3.0)
    si.drawLine(gc, xPosition_pix, rect.y, xPosition_pix, rect.y + rect.height)

    ImageUtil.setLineThickness(gc, 1.0)
    pixelWiseEdgeList_pix.indices.foreach(index => drawSinglePixelOfLeafEdge(index, pixelWiseEdgeList_pix(index), stakittAOI))

    annotateLeafEnd(xPosition_pix, stakittAOI)

    xPosition_pix
  }

  private def constructStakittResult(stakittAOI: StakittAOI): StakittResult = {
    val edgePosition_pix = measureLeafEnd(stakittAOI)
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

  def measureLeafPositions(): Seq[StakittResult] = {

    // val j = (0 until xAoiPairList.size).map(doColumn)

    val aoiList = StakittAOI.makeAOIs(xImageBorders, yImageBorders, rtimage)

    aoiList.map(constructStakittResult)

    Trace.showInMSPaint(bufImg) // TODO rm

    Seq()

  }

}
