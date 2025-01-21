package org.aqa.webrun.wl.wlMonthly

import org.aqa.db.WinstonLutz
import org.aqa.web.WebUtil
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.Logging

import scala.xml.Elem

object WLRunMonthly extends Logging {

  /**
    * Perform monthly processing if the required data is there.
    * @param extendedData metadata
    * @param runReq DICOM slices
    * @param dbList analysis results.
    * @return HTML snippet for access to results.
    */
  def run(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz]): Elem = {

    val pairList = WLBeam.makePairList(runReq, dbList)

    val monthly = WLMonthly.make(extendedData, pairList)
    val table = WLTable.make(extendedData, pairList)
    val collimator = WLCollimator.make(extendedData, pairList)

    // only do this if the required data is there.
    if (monthly.isDefined && table.isDefined && collimator.isDefined) {

      val wlMonthlyHTML = org.aqa.webrun.wl.wlMonthly.wlMonthlyHTML.WLMonthlyHTML(extendedData, pairList, monthly.get, table.get, collimator.get)

      val htmlRef = wlMonthlyHTML.mainPage()

      val elem = {
        val nl = WebUtil.titleNewline
        val title = s"""To re-calculate spreadsheet in Excel, use$nl CTRL-ALT-SHIFT-F9,and then run the solver on$nl each of the Analysis and Collimator sheets."""
        <div style="text-align: center; border: 2px solid #777777; margin-top:10px;" title={title}>
          <div style="text-align: center; border: 1px solid lightgrey; margin10px;">
            {htmlRef}
          </div>
        </div>
      }

      elem
    } else {
      // not a monthly data set
      <span></span>
    }
  }

}
