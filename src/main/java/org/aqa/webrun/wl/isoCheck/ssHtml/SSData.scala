package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLColumn
import org.aqa.webrun.wl.isoCheck.WLColumnAlAnonText
import org.aqa.webrun.wl.isoCheck.WLColumnMachine
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.blankCells
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssDataLeft
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssDataRight
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssPreprocessRight
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.wl.isoCheck.WLMap

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  * @param extendedData Metadata
  * @param pairList WL data
  */
class SSData(extendedData: ExtendedData, runReq: WLRunReq, wlMap: WLMap, isoTable: Option[WLIsoTable]) extends SSSheet {

  override val name: String = "Data"

  val columnList: Seq[WLColumn] = org.aqa.webrun.wl.isoCheck.WLColumnList(extendedData.machine, wlMap.list.head.dataDate).columnList

  private def toPlainHtml(text: String, alignLeft: Boolean = true): Elem = {
    val a = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    <td class={a}>
      {text}
    </td>
  }

  private def toHtml(text: String, alignLeft: Boolean = true, alias: Boolean = false): Elem = {
    val a = if (alignLeft) cssDataLeft else cssDataRight

    if (alias) {
      <td class={a}>
        {WebUtil.wrapAlias(text)}
      </td>
    } else {
      <td class={a}>
        {text}
      </td>
    }
  }

  private def makeTitleRow: Elem = {

    val title = "Winston-Lutz Field Data"

    val dataDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)

    val analysisDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.analysisDate.get)

    <tr>
      {WLXlsxUtil.makeRowIndex(1)}
      {Seq(title, dataDateText, analysisDateText).map(text => toHtml(text))}
      {(0 until 23).map(_ => toHtml(""))}
    </tr>
  }

  private def makeHeaderRow: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(2)}
      {columnList.map(col => toHtml(col.name))}
      {toHtml("")}
    </tr>
  }

  private def makeRow(index: Int): Elem = {

    val wl = wlMap.list(index)

    def colToHtml(col: WLColumn): Elem = {
      toHtml(col.toText(wl, runReq.alOf(wl).get), col.alignLeft, alias = col.isInstanceOf[WLColumnAlAnonText] || col.isInstanceOf[WLColumnMachine])
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(index + 3)}
      {columnList.map(colToHtml)}
      {toHtml("")}
    </tr>
  }

  private def blankRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {blankCells(26)}
    </tr>
  }

  //noinspection SameParameterValue
  private def ballCbctRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      <td>ballCBCT</td>
      {blankCells(25)}
    </tr>
  }

  //noinspection SameParameterValue
  private def xyzMmRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toPlainHtml("x (mm)")}
      {toPlainHtml("y (mm)")}
      {toPlainHtml("z (mm)")}
      {blankCells(23)}
    </tr>
  }

  //noinspection SameParameterValue
  private def zeroRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toPlainHtml("0", alignLeft = false)}
      {toPlainHtml("0", alignLeft = false)}
      {toPlainHtml("0", alignLeft = false)}
      {blankCells(23)}
    </tr>
  }

  private def zeroDataRow(index: Int, name: String): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toHtml("0", alignLeft = false)}
      {toHtml("0", alignLeft = false)}
      {toHtml("0", alignLeft = false)}
      {toHtml(name)}
      {blankCells(22)}
    </tr>
  }

  private def makeCbct: Seq[Elem] = {
    Seq(
      blankRow(18),
      ballCbctRow(19),
      xyzMmRow(20),
      zeroRow(21),
      blankRow(22),
      zeroDataRow(23, "AcqIsocenter"),
      zeroDataRow(24, "Ball"),
      blankRow(25)
    )
  }

  private val isoTableFiller: Seq[Elem] = {
    0 match {
      case _ if isoTable.isEmpty            => (12 to 17).map(blankRow) // no table data
      case _ if isoTable.get.T_60.isDefined => Seq() //                          six beams of table data
      case _                                => (14 to 17).map(blankRow) // two beams of table data
    }
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {WLXlsxUtil.makeAlphaRow(26)}
        {makeTitleRow}
        {makeHeaderRow}
        {wlMap.list.indices.map(makeRow)}
        {isoTableFiller}
        {makeCbct}
      </table>
    }

    content
  }
}
