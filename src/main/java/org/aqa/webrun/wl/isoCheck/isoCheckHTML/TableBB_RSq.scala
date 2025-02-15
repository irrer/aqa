package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.C3ChartHistory
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML.tdElem

import scala.xml.Elem

class TableBB_RSq(isoCheckChart: WLIsoCheckChart, isoTable: Option[WLIsoTable]) {

  private def tdMake(tableAngle: Int): Elem = {
    val name = "T" + tableAngle
    val it = isoTable.get
    val beam = it.beamList.find(bb => bb.tableAngle == tableAngle).get
    val value = it.BB_Rpp(beam, it.get_dXT__0_Optimized, it.get_dZT__0_Optimized, it.get_IsoTable_X_Optimized, it.get_IsoTable_Z_Optimized)
    tdElem(name, value)
  }

  private def tableBB_RSqTable: Elem = {

    <table class="table table-bordered" style="text-align: center;">
      <tr>
        {tdMake(0)}
        {tdMake(30)}
        {tdMake(60)}
        {tdMake(90)}
        {tdMake(270)}
        {tdMake(300)}
        {tdMake(330)}
      </tr>
    </table>
  }

  val content: Seq[Elem] = {
    if (isoCheckChart.isoTableBB_RSqChart.isDefined) {
      Seq(
        <div style="margin-top:100px;">
          <hr style="border:3px solid #777777; border-radius: 8px;" > </hr>
          <h3>Table BB-R"^2</h3>
          {tableBB_RSqTable}
          {C3ChartHistory.htmlRef(isoCheckChart.isoTableBB_RSqChart.get.chartIdOpt.get)}
        </div>
      )
    } else
      Seq()
  }
}
