package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil
import org.aqa.Logging
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML

import java.io.File
import scala.xml.Elem

/**
  * Handle construction of the tabs for the HTML version of the spreadsheet.  Each requires
  * two parts, the tab, and the content for the tab.
  * @param sheet One sheet of the spreadsheet.
  * @param sheetList list of all sheets.
  */
private case class Tab(sheet: SSSheet, sheetList: Seq[SSSheet]) {
  private val name: String = sheet.name
  private val content: Elem = sheet.make()
  private val id: String = C3Chart.makeUniqueChartIdTag

  private val isFirst: Boolean = name.equals(sheetList.head.name)

  def toListItem: Elem = {
    <li class={if (isFirst) "active" else ""} style="">
      <a data-toggle="tab" href={s"#$id"} style="text-align:center;">
        {name}
      </a>
    </li>
  }

  def toContent: Elem = {
    val cls = {
      if (isFirst)
        "tab-pane fade in active"
      else
        "tab-pane fade"
    }

    <div id={id} class={cls}>
      {content}
    </div>
  }
}

/**
  * Make a web page containing all the spreadsheets.
  */
object SSHtml extends Logging {



  /**
    * Make a web page containing all the spreadsheets.
    * @param extendedData Metadata.
    * @param pairList List of images with analysis.
    * @return name of CSV file.
    */
  def make(extendedData: ExtendedData, pairList: Seq[WLBeam], isoCheck: WLIsoCheck, isoTable: WLIsoTable, collimator: WLCollimator): String = {

    val ssSNCImport = new SSSNCImport(extendedData: ExtendedData, isoCheck, collimator, isoTable)
    val ssReport = new SSReport(extendedData: ExtendedData, isoCheck, isoTable)

    val sheetList: Seq[SSSheet] = Seq(
      ssSNCImport,
      new SSData(extendedData: ExtendedData, pairList),
      new SSPreprocess(extendedData: ExtendedData, pairList),
      new SSAnalysis(extendedData: ExtendedData, isoCheck, isoTable),
      new SSCollimator(extendedData: ExtendedData, collimator),
      ssReport,
      new SSInstructions()
    )

    val tabList = sheetList.map(sheet => Tab(sheet, sheetList))

    def makeContent(): Elem = {
      <div>

        <div class="row">
          <div class="col-md-2">
            {WebUtil.showPrecision}
          </div>
          <div class="col-md-2 col-md-offset-1">
            <h2 style="margin:10px;">IsoCheck</h2>
          </div>
        </div>

        <ul class="nav nav-tabs">
          {tabList.map(_.toListItem)}
        </ul>
        <div class="tab-content" style="margin-right:20px;">
          {tabList.map(_.toContent)}
        </div>
      </div>
    }

    val content = makeContent()

    val baseFileName = WLXlsxUtil.baseFileName(extendedData)

    val htmlFile = new File(WLIsoCheckHTML.dir(extendedData), s"$baseFileName.html")

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "WL IsoCheck", c3 = true, runScript = Some(ssReport.js))

    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote spreadsheet as HTML to ${htmlFile.getAbsolutePath}")

    htmlFile.getName
  }

}
