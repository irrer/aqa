package org.aqa.webrun.wl.wlMonthly.wlMonthlyHTML

import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLCollimator
import org.aqa.webrun.wl.wlMonthly.WLTable
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.wlMonthly.WLBeam
import org.aqa.webrun.wl.wlMonthly.WLMonthly
import org.aqa.webrun.wl.wlMonthly.WLXLSXSpreadsheet
import org.aqa.webrun.wl.wlMonthly.ssHtml.SSHtml

import java.io.File
import scala.xml.Elem

/**
  * Make the HTML for the main page of the monthly report.
  *
  * @param extendedData Metadata
  * @param pairList List of beam results and DICOM
  * @param monthly Monthly data.
  * @param table Table data.
  * @param collimator Collimator data.
  */

case class WLMonthlyHTML(extendedData: ExtendedData, pairList: Seq[WLBeam], monthly: WLMonthly, table: WLTable, collimator: WLCollimator) extends Logging {

  def mainPage(): Elem = {

    val gradient = WLGradientHTML(extendedData, monthly, table, collimator)

    val gradientHtml = gradient.makeHtml()

    val htmlFile = new File(WLMonthlyHTML.dir(extendedData), "index.html")

    val fileNameHtmlSpreadsheet = SSHtml.make(extendedData, pairList, monthly, table, collimator)

    val fileName_xlsx = WLXLSXSpreadsheet.makeSpreadsheet(extendedData, pairList, table, collimator)

    val content = {
      <div>
        <a href={fileNameHtmlSpreadsheet}>View Spreadsheet</a>
        <br></br>
        <a href={fileName_xlsx} style="margin-left:50px;">Download XLSX WLCollimator</a>
        <br></br>
        {gradientHtml}
      </div>
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "WL Monthly", runScript = None)
    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote spreadsheet as HTML to ${htmlFile.getAbsolutePath}")

    val htmlRef = {
      val hRef = WLMonthlyHTML.dirName + "/" + htmlFile.getName
      <h4><a href={hRef} style="margin:20px;">WL Monthly</a></h4>
    }
    htmlRef
  }

}

object WLMonthlyHTML extends Logging {

  /** Name of monthly dir */
  val dirName = "WLMonthly"

  /**
    * Monthly dir.
    * @param extendedData Metadata.
    * @return Monthly dir.
    */
  def dir(extendedData: ExtendedData): File = {
    val d = new File(extendedData.output.dir, dirName)
    if (d.mkdirs())
      logger.info(s"Created WL Monthly HTML dir ${d.getAbsolutePath}")
    d
  }
}
