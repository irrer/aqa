package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.C3ChartHistory
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML.tdElem

import scala.xml.Elem

class TableBB_RSqSevenBeams(isoCheckChart: WLIsoCheckChart, isoTable: Option[WLIsoTable]) {

  private def tdMake(tableAngle: Int): Option[Elem] = {
    val name = "T" + tableAngle
    val it = isoTable.get
    val wl = it.beamList.find(bb => bb.tableAngleRounded.isDefined && (bb.tableAngleRounded.get == tableAngle))
    if (wl.isDefined) {
      val value = it.BB_Rpp(wl.get, it.get_dXT__0_Optimized, it.get_dZT__0_Optimized, it.get_IsoTable_X_Optimized, it.get_IsoTable_Z_Optimized)
      Some(tdElem(name, value))
    } else
      None
  }

  private def tableBB_RSqTable: Elem = {

    <table class="table table-bordered" style="text-align: center;">
      <tr>
        {Seq(0, 30, 60, 90, 270, 300, 330).flatMap(tdMake)}
      </tr>
    </table>
  }

  val content: Seq[Elem] = {
    if (isoTable.isDefined && isoCheckChart.isoTableBB_RSqChartSevenBeams.isDefined) {
      Seq(
        <div style="margin-top:100px;">
          <hr style="border:3px solid #777777; border-radius: 8px;" > </hr>
          <h3>Table BB-R"^2 Seven Beams</h3>
          {tableBB_RSqTable}
          {C3ChartHistory.htmlRef(isoCheckChart.isoTableBB_RSqChartSevenBeams.get.chartIdOpt.get)}
        </div>
      )
    } else
      Seq()
  }
}
