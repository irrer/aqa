package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.C3ChartHistory
import org.aqa.web.WebUtil
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML.tdElem

import scala.xml.Elem

class TableGantryAndCollimator(isoCheckChart: WLIsoCheckChart, isoCheck: WLIsoCheck, collimator: WLCollimator) {

  private val table: Elem = {
    <table class="table table-bordered" style="text-align: center;">
      <tr>
        {tdElem("CBCT - Gantry Iso X", -isoCheck.isoX)}
        {tdElem("CBCT - Gantry Iso Y", -isoCheck.isoY)}
        {tdElem("CBCT - Gantry Iso Z", -isoCheck.isoZ)}
        {tdElem("Gantry Flex", isoCheck.gantryFlex)}
      </tr>
      <tr>
        {tdElem("Col-Gantry misalignment", isoCheck.collGantryMisalign)}
        {tdElem("MLC offset", isoCheck.mlcOffsetY)}
        {tdElem("Gantry Isocentricity", isoCheck.gantryIsocentricity)}
        {tdElem("Collimator Isocentricity", collimator.getCA_Rpp_Optimized)}
      </tr>
    </table>
  }

  val content: Elem = {
    <div style="margin-top:40px;">
      <hr style="border:3px solid #777777; border-radius: 8px;" > </hr>
      {WebUtil.showPrecision}
      <h3>Gantry and Collimator</h3>
      {table}
      {C3ChartHistory.htmlRef(isoCheckChart.isoCheckChart.chartIdOpt.get)}
    </div>
  }

}
