package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.webrun.stakitt.Analysis
import org.aqa.Logging
import org.aqa.web.WebUtil
import org.aqa.webrun.stakitt.StakittResult
import org.aqa.Util

import scala.xml.Elem

/**
 * Construct the main HTML table for Stakitt.
 *
 * @param analysis Results of Stakitt analysis.
 */
case class HtmlTable(analysis: Analysis) extends Logging {

  private def fmt(d: Double) = {
    if (d.round == d)
      d.round.toString
    else
      Util.fmtDbl(d)
  }

  private val xIndexList = analysis.stakittList.map(_.stakittAOI.xIndex).distinct.sorted

  private def makeTableHead(): Elem = {

    def makeColumn(xIndex: Int): Elem = {
      val result = analysis.stakittList.find(r => r.stakittAOI.xIndex == xIndex).get
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
      val elem = {
        <td style="text-align: center;">
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

    <table class="table table-bordered" title="Leaf end offsets in mm.">
      {makeTableHead()}
      {yIndexList.map(makeRow)}
    </table>
  }

}
