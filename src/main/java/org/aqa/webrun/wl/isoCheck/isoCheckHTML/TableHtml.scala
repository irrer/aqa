package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.C3ChartHistory
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML.tdElem

import scala.xml.Elem

class TableHtml(isoCheckChart: WLIsoCheckChart, isoCheck: WLIsoCheck, isoTable: Option[WLIsoTable]) {

  private def table: Elem = {
    <table class="table table-bordered" style="text-align: center;">
      <tr>
        {tdElem("Table - Gantry Iso X", isoTable.get.get_IsoTable_X_Optimized - isoCheck.isoX)}
        {tdElem("Table - Gantry Iso Z", isoTable.get.get_IsoTable_Z_Optimized - isoCheck.isoZ)}
        {tdElem("Table Isocentricity", Math.sqrt(isoTable.get.get_RSquared_Optimized))}
      </tr>
    </table>
  }

  val content: Seq[Elem] = {
    if (isoTable.isDefined && isoCheckChart.isoTableChart.isDefined) {
      val elem = {
        <div style="margin-top:100px;">
          <hr style="border:3px solid #777777; border-radius: 8px;" > </hr>
          <h3>Table</h3>
          {table}
          {C3ChartHistory.htmlRef(isoCheckChart.isoTableChart.get.chartIdOpt.get)}
        </div>
      }
      Seq(elem)
    } else
      Seq()
  }
}
