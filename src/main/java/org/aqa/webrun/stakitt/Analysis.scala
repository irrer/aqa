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

case class Analysis(extendedData: ExtendedData, rtimage: AttributeList, rtplan: Option[AttributeList]) extends Logging {

  private val dicomImage = new DicomImage(rtimage)

  // private val beamName: String = Phase2Util.getBeamNameOfRtimage(rtplan, rtimage).get

  // private case class AOIBorder(lo: Double, hi: Double) {}

  // private case class AOIBorderList(staggeredLo: AOIBorder, smooth: Seq[AOIBorder], staggeredHi: AOIBorder) {}

  private val xImageBorders = LeafEnds(rtimage)
  private val yImageBorders = LeafBoundaries(rtimage, xImageBorders.xPointList)

  private val planBorders = rtplan.map(p => PlanBorders(rtimage, p))

  def analyze(): Unit = { // StakittResult = { TODO
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

    xImageBorders.xPointList.foreach(vertLine)

    // horzLine(yImageBorders.yPointList_pix, "All", 1, Color.white)
    horzLine(yImageBorders.yPointListLo_pix, "Lo", 2, Color.black)
    horzLine(yImageBorders.yPointListHi_pix, "Hi", 3, Color.white)

    val leafEndPositionList = new LeafEndPositions(dicomImage, xImageBorders, yImageBorders, rtimage).measureLeafPositions()

    // StakittResult(extendedData, rtimage, rtplan)
    Trace.showInMSPaint(bufImg)

    // StakittResult(extendedData, rtimage, rtplan)
  }
}
