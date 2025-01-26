package org.aqa.webrun.wl.isoCheck

import org.aqa.Logging

object WLXlsx extends Logging {

  /*
  def make(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz]): File = {

    val workbook = new SXSSFWorkbook

    val spreadSheetList = Seq(
      // SSSncImport(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz]),
      // WLData(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz], workbook),
      WLPreprocess(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz], workbook)
    )

    case class Tab(sheetMaker: WLSheetMaker) {
      private val name: String = sheetMaker.sheetName
      private val content: Elem = sheetMaker.make()
      private val id: String = C3Chart.makeUniqueChartIdTag

      private val isFirst: Boolean = name.equals(spreadSheetList.head.sheetName)

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

    val tabList = spreadSheetList.map(Tab)

    def makeContent(): Elem = {

      <div>
        <ul class="nav nav-tabs">
          {tabList.map(_.toListItem)}
        </ul>
        <div class="tab-content">
          {tabList.map(_.toContent)}
        </div>
      </div>

    }

    val content = makeContent()

    val baseFileName = {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd'_'HH-mm")
      val dateText = Util.formatDate(dateFormat, extendedData.output.dataDate.get)
      s"WinstonLutz_$dateText"
    }

    val htmlFile = new File(extendedData.output.dir, s"$baseFileName.html")

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "WL IsoCheck", runScript = None)

    Util.writeFile(htmlFile, text)
    logger.info(s"Wrote spreadsheet as HTML to ${htmlFile.getAbsolutePath}")

    val xlsxFile = new File(extendedData.output.dir, s"$baseFileName.xlsx")

    workbook.write(new FileOutputStream(xlsxFile))
    logger.info(s"Wrote spreadsheet as HTML to ${xlsxFile.getAbsolutePath}")

    htmlFile
  }
   */

}
