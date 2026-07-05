package org.aqa.webrun.stakitt.stakittHTML

import edu.umro.ImageUtil.ImageUtil
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Logging
import org.aqa.web.WebUtil
import org.aqa.webrun.stakitt.StakittResult
import org.aqa.Util

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import scala.xml.Elem

/**
  * Construct the main HTML table for Stakitt.
  *
  * @param analysis Results of Stakitt analysis.
  */
case class HtmlTable(analysis: Analysis, dir: File) extends Logging {

  private def fmt(d: Double) = {
    if (d.round == d)
      d.round.toString
    else
      Util.fmtDbl(d)
  }

  private def rgbToHtml(rgb: Int): String = {
    val txt = "%08x".format(rgb & 0xffffff)
    "#" + txt.takeRight(6)
  }

  // ----------------------------------------------------------------------------------------

  private val minOffset = analysis.stakittList.map(r => r.stakitt.leafEndOffset_mm).min
  private val maxOffset = analysis.stakittList.map(r => r.stakitt.leafEndOffset_mm).max
  private val medianOffset = (minOffset + maxOffset) / 2

  private val palette: IndexedSeq[Int] = {
    ImageUtil.rgbColorMap(Color.white)
  }

  private val clrA = (palette.size - 1) / (maxOffset - minOffset)
  private val clrB = -(clrA * minOffset)

  private def colorOfCell(offset: Double): Int = {
    val index = Math.clamp(((offset * clrA) + clrB).round.toInt, 0, palette.size - 1)
    val clr = palette(Math.clamp(index, 0, palette.size - 1))
    clr
  }

  private def colorOfCellHtml(offset: Double): String = {
    rgbToHtml(colorOfCell(offset))
  }

  /**
    * Given a color, convert it to the corresponding value.
    * @param colorIndex Position on color palette
    * @return offset value
    */
  private def valueOfColor(colorIndex: Int): Double = {
    val offset = (colorIndex - clrB) / clrA
    offset
  }

  // ----------------------------------------------------------------------------------------

  private def legend(): Elem = {

    // height of legend in pixels
    val height = 10

    val bufImg = new BufferedImage(palette.size, height, BufferedImage.TYPE_INT_RGB)

    def setColor(i: Int): Unit = {
      val color = colorOfCell(valueOfColor(i))
      (0 until height).foreach(y => bufImg.setRGB(i, y, color))
    }

    palette.indices.foreach(setColor)

    val file = new File(dir, "legend.png")
    Util.writePng(bufImg, file)
    logger.info("Wrote file " + file.getAbsolutePath)

    val elem = {
      val style = s"display: grid; place-items: center; border: 1px solid lightgrey;"
      <div style={style}>
        <div style="margin:5px;">
          <b>
            <div style="display:flex; justify-content:space-between; width:100%;">
              <span> </span>
              <span>Low values are dark, high values are light.</span>
              <span> </span>
            </div>
            <div style="display:flex; justify-content:space-between; width:100%;">
              <i>Min</i>
              <i>Median</i>
              <i>Max</i>
            </div>
            <div style="display:flex; justify-content:space-between; width:100%;">
              {WebUtil.setPrecisionAttr(<span></span>,minOffset)}
              {WebUtil.setPrecisionAttr(<span></span>,medianOffset)}
              {WebUtil.setPrecisionAttr(<span></span>,maxOffset)}
            </div>
          </b>
          <img src={dir.getName + "/" + file.getName} class="img-responsive fit-image" style="width:846px; height:auto; display:block;"/>
        </div>
      </div>
    }

    elem
  }

  // ----------------------------------------------------------------------------------------

  private val xIndexList = analysis.stakittList.map(_.stakittAOI.xIndex).distinct.sorted

  private def makeTableHead(): Elem = {

    def makeColumn(xIndex: Int): Elem = {
      val result =
        analysis.stakittList.find(r => r.stakittAOI.xIndex == xIndex).get
      <th>{fmt(result.stakitt.plannedEndPosition_mm) + " mm"}</th>
    }

    <thead title="Planned leaf end position.">
      <tr>
        {xIndexList.map(makeColumn)}
      </tr>
    </thead>
  }

  private def makeRow(yIndex: Int): Elem = {

    val rowMembers = analysis.stakittList.filter(_.stakittAOI.yIndex == yIndex).sortBy(_.stakittAOI.xIndex)

    def makeColumn(result: StakittResult): Elem = {
      val offset = result.stakitt.leafEndOffset_mm
      val textColor = {
        val rgb = if (offset >= medianOffset) palette.head else palette.last
        rgbToHtml(rgb)
      }
      val elem = {
        val style = s"font-size:10px; text-align: center; color:$textColor; background-color:${colorOfCellHtml(offset)}"
        <td style={style}>
          {"%8.3f".format(offset).trim}
        </td>
      }

      WebUtil.setPrecisionAttr(elem, result.stakitt.leafEndOffset_mm)
    }

    <tr>
      {rowMembers.map(makeColumn)}
    </tr>
  }

  def mainTable(): Elem = {

    val yIndexList = analysis.stakittList.map(_.stakittAOI.yIndex).distinct.sorted

    <div>
      {legend()}
      <table class="table table-bordered" title="Leaf end offsets in mm.">
        {makeTableHead()}
        {yIndexList.map(makeRow)}
      </table>
    </div>

  }

}
