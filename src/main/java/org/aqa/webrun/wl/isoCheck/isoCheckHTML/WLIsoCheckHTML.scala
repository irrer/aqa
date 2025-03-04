package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.WLXLSXSpreadsheet
import org.aqa.webrun.wl.isoCheck.ssHtml.SSHtml
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.wl.isoCheck.WLMap

import java.io.File
import scala.xml.Elem

/**
  * Make the HTML for the main page of the isoCheck report.
  *
  * @param extendedData Metadata
  * @param runReq Raw input data
  * @param wlMap List of beam results and DICOM
  * @param isoCheck IsoCheck data.
  * @param isoTable IsoTable data.
  * @param collimator Collimator data.
  */

case class WLIsoCheckHTML(extendedData: ExtendedData, runReq: WLRunReq, wlMap: WLMap, isoCheck: WLIsoCheck, collimator: WLCollimator, isoTable: Option[WLIsoTable]) extends Logging {

  def mainPage(): Elem = {

    val gradient = WLGradientHTML(extendedData, isoCheck, isoTable, collimator)

    val gradientHtml = gradient.makeHtml()

    val htmlFile = new File(WLIsoCheckHTML.dir(extendedData), "index.html")

    val fileNameCsvSNCImport = SSHtml.make(extendedData, runReq, wlMap, isoCheck, isoTable, collimator)

    val fileName_xlsx = WLXLSXSpreadsheet.makeSpreadsheet(extendedData, runReq, wlMap, isoTable, collimator)

    val isoCheckChart = new WLIsoCheckChart(extendedData.outputPK)

    val mlcWobbleChart = MLCWobbleChart.makeChart(isoCheck)

    val tableWobbleChart = if (isoTable.isDefined) Some(TableWobbleChart.makeChart(isoTable)) else None

    val tableWobbleChartJs = if (tableWobbleChart.isDefined) tableWobbleChart.get.javascript else ""

    val wobbleCharts: Elem = {

      val t: Elem = if (tableWobbleChart.isDefined) tableWobbleChart.get.html else <span>Not Available</span>

      val content = {
        <div class="row">
          <div class="col-md-6" style="border: 2px solid black;">
            <h4 style="text-align: center;">MLC Wobble about Collimator Axis</h4>
            {mlcWobbleChart.html}
          </div>
          <div class="col-md-6" style="border: 2px solid black;">
            <h4 style="text-align: center;">IsoTable Wobble about IsoTable Axis</h4>
            {t}
          </div>
        </div>
      }
      content
    }

    val linkTable = {

      <table class="table table-bordered" style="margin-top:10px;text-align: center;">
        <tr>
          <td title="View an HTML version of a spreadsheet showing detailed results.">
            <a href={SSHtml.spreadsheetHtmlFileName}>View Spreadsheet</a>
          </td>
          <td title="Download a CSV file suitable for import into SNC SunCheck.">
            <a href={fileNameCsvSNCImport} style="margin-left:50px;">Download CSV for SNC</a>
          </td>
          <td title={"Download an XLSX spreadsheet showing detailed results.  After loading, click CTRL-ALT-F9 to calculate results."}>
            <a href={"/WLIsoCheckDownloadXLSX?outputPK=" + extendedData.outputPK} style="margin-left:50px;">Download XLSX</a>
          </td>
          <td title="Show images of gradient descent.">
            {gradientHtml}
          </td>
        </tr>
      </table>
    }

    val maxR = Seq(Some(isoCheck.maxR), isoTable.map(_.maxR)).flatten.max
    val tableGantryAndCollimatorHtml = new TableGantryAndCollimator(isoCheckChart, isoCheck, collimator, maxR)
    val tableHtml: TableHtml = new TableHtml(isoCheckChart, isoCheck, isoTable)
    val tableBB_RSqThreeBeams = new TableBB_RSqThreeBeams(isoCheckChart, isoTable)
    val tableBB_RSqSevenBeams = new TableBB_RSqSevenBeams(isoCheckChart, isoTable)

    val content = {
      <div>
        <h2>IsoCheck</h2>
        {linkTable}
        {wobbleCharts}
        {tableGantryAndCollimatorHtml.content}
        {tableHtml.content}
        {tableBB_RSqThreeBeams.content}
        {tableBB_RSqSevenBeams.content}
        <div style="margin-bottom:300px;"> </div>
      </div>
    }

    val runScript = {
      val scatter = s"""<script>${mlcWobbleChart.javascript}\n$tableWobbleChartJs</script>"""
      val trend = s"""<script src='/WLIsoCheckChartHistoryRestlet?outputPK=${extendedData.outputPK}'></script>\n"""
      scatter + trend
    }

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "IsoCheck", c3 = true, runScript = Some(runScript))
    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote spreadsheet as HTML to ${htmlFile.getAbsolutePath}")

    val htmlRef = {
      val hRef = WLIsoCheckHTML.dirName + "/" + htmlFile.getName
      <h4><a href={hRef} style="margin:20px;">IsoCheck</a></h4>
    }
    htmlRef
  }

}

object WLIsoCheckHTML extends Logging {

  /** Name of isoCheck dir */
  val dirName = "WLIsoCheck"

  /**
    * IsoCheck dir.
    * @param extendedData Metadata.
    * @return IsoCheck dir.
    */
  def dir(extendedData: ExtendedData): File = {
    val d = new File(extendedData.output.dir, dirName)
    if (d.mkdirs())
      logger.info(s"Created WL IsoCheck HTML dir ${d.getAbsolutePath}")
    d
  }

  def tdElem(name: String, value: Double): Elem = {
    <td>{name}: {WebUtil.setPrecisionAttr(<span></span>, value)}</td>
  }

}
