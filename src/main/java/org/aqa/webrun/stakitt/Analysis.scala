package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.ScaledImage
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.webrun.stakitt.leafBoundaries.LeafBoundaries

import java.awt.Color

case class Analysis(dicomImage: DicomImage, xAOIBorders: LeafEnds, yAOIBorders: LeafBoundaries, leafEndPositions: Seq[StakittResult]) extends Logging {

  private def showAOIBorderSpans(): Unit = {

    val scale = 7
    val bufImg = {
      val img = dicomImage.toDeepColorBufferedImage(0.01)
      ImageUtil.magnify(img, scale)
    }
    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    val si = ScaledImage(scale, 0, 0)

    def vertLine(x: Double): Unit = si.drawLine(gc, x, 0, x, dicomImage.height)

    def horzLine(lineList: Seq[Double], name: String, offset: Int, color: Color): Unit = {
      gc.setColor(color)
      val rect = ImageText.getTextDimensions(gc, name)
      ImageText.drawTextCenteredAt(gc, (rect.getWidth / 2) + 20, (rect.getHeight + 1) * 2 * offset, name)
      lineList.foreach(y => si.drawLine(gc, 0, y, dicomImage.width, y))
    }

    xAOIBorders.xPointList.foreach(vertLine)

    // horzLine(yImageBorders.yPointList_pix, "All", 1, Color.white)
    horzLine(yAOIBorders.yPointListLo_pix.adjusted_pix, "Lo", 2, Color.black)
    horzLine(yAOIBorders.yPointListHi_pix.adjusted_pix, "Hi", 3, Color.white)

    Trace.showInMSPaint(bufImg)
  }

  // enable this to see the leaf boundaries
  if (false) showAOIBorderSpans()
}

object Analysis extends Logging {

  def analyze(extendedData: ExtendedData, rtimage: AttributeList, rtplan: Option[AttributeList]): Analysis = {

    val dicomImage = new DicomImage(rtimage)

    val xAOIBorders = LeafEnds(rtimage)
    val yAOIBorders = LeafBoundaries(rtimage, xAOIBorders.xPointList)

    val planAOIBorders = rtplan.map(p => PlanBorders(rtimage, p)) // TODO require?

    val leafEndPositionList = LeafEndPositions(extendedData, dicomImage, xAOIBorders, yAOIBorders, rtimage).measureLeafPositions()

    val analysis = Analysis(dicomImage, xAOIBorders, yAOIBorders, leafEndPositionList)

    analysis
  }

}
