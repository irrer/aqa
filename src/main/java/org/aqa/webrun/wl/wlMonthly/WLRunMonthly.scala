package org.aqa.webrun.wl.wlMonthly

import org.aqa.db.WinstonLutz
import org.aqa.web.WebUtil
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.ssHtml.SSHtml
import org.aqa.Logging

import scala.xml.Elem

object WLRunMonthly extends Logging{

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

    // only do this if the required data is there.
    if (monthly.isDefined && table.isDefined) {

      val ssFileName = WLData.makeSpreadsheet(extendedData, pairList, table.get)
      val htmlFileName = SSHtml.make(extendedData, pairList, monthly.get, table.get)

      val elem = {

        val nl = WebUtil.titleNewline
        val title = s"""To re-calculate spreadsheet in Excel, use$nl CTRL-ALT-SHIFT-F9,and then run the solver on$nl each of the Analysis and Collimator sheets."""
        <div style="text-align: center; border: 1px solid lightgrey; margin-top:10px;" title={title}>
          <table style="margin-left:10px;margin-right:10px;">
            <tr>
              <td colspan="2" style="text-align: center;"><b> Monthly Spreadsheet </b></td>
            </tr>
            <tr>
              <td style="text-align: center;"><a href={htmlFileName}>View</a></td>
              <td style="text-align: center;"><a href={ssFileName}>Download</a></td>
            </tr>
          </table>
        </div>
      }

      elem
    } else {
      // not a monthly data set
      <span></span>
    }
  }

}
