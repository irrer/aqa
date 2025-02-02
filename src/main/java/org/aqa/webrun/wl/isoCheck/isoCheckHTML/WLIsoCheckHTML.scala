package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import edu.umro.ScalaUtil.Trace
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.isoCheck.IsoCheck
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLXLSXSpreadsheet
import org.aqa.webrun.wl.isoCheck.ssHtml.SSHtml

import java.io.File
import scala.xml.Elem

/**
  * Make the HTML for the main page of the isoCheck report.
  *
  * @param extendedData Metadata
  * @param pairList List of beam results and DICOM
  * @param isoCheck IsoCheck data.
  * @param isoTable IsoTable data.
  * @param collimator Collimator data.
  */

case class WLIsoCheckHTML(extendedData: ExtendedData, pairList: Seq[WLBeam], isoCheck: IsoCheck, isoTable: Option[WLIsoTable], collimator: WLCollimator) extends Logging {

  def mainPage(): Elem = {

    val gradient = WLGradientHTML(extendedData, isoCheck, isoTable, collimator)

    val gradientHtml = gradient.makeHtml()

    val htmlFile = new File(WLIsoCheckHTML.dir(extendedData), "index.html")

    val fileNameCsvSNCImport = SSHtml.make(extendedData, pairList, isoCheck, isoTable, collimator)

    Trace.trace()
    val fileName_xlsx = WLXLSXSpreadsheet.makeSpreadsheet(extendedData, pairList, isoTable, collimator)
    Trace.trace()

    val content = {
      <div>
        <table>
          <tr>
            <td>
              <a href={SSHtml.spreadsheetHtmlFileName}>View Spreadsheet</a>
            </td>
            <td>
              <a href={fileNameCsvSNCImport} style="margin-left:50px;">Download CSV for SNC</a>
            </td>
            <td>
              <a href={fileName_xlsx} style="margin-left:50px;">Download XLSX</a>
            </td>
            <td>
              {gradientHtml}
            </td>
          </tr>
        </table>
      </div>
    }

    Trace.trace()
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "IsoCheck", runScript = None)
    Trace.trace()
    Util.writeFile(htmlFile, text)
    Trace.trace()
    logger.info(s"Wrote spreadsheet as HTML to ${htmlFile.getAbsolutePath}")
    Trace.trace()

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
}
