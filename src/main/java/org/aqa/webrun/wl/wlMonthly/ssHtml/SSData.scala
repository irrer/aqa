package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.Util
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLBeam
import org.aqa.webrun.wl.wlMonthly.WLColumn
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssDataLeft
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssDataRight
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessRight

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  * @param extendedData Metadata
  * @param pairList WL data
  */
class SSData(extendedData: ExtendedData, pairList: Seq[WLBeam]) extends SSSheet {

  override val name: String = "Data"

  val columnList: Seq[WLColumn] = org.aqa.webrun.wl.wlMonthly.WLColumnList(extendedData.machine, pairList.head.acquisition).columnList

  private def toPlainHtml(text: String, alignLeft: Boolean = true): Elem = {
    val a = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    <td class={a}>
      {text}
    </td>
  }

  private def toHtml(text: String, alignLeft: Boolean = true): Elem = {
    val a = if (alignLeft) cssDataLeft else cssDataRight
    <td class={a}>
      {text}
    </td>
  }

  private def makeTitleRow: Elem = {

    val title = "Winston-Lutz Field Data"

    val dataDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)

    val analysisDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.analysisDate.get)

    <tr>
      {WLXlsxUtil.makeRowIndex(1)}
      {Seq(title, dataDateText, analysisDateText).map(text => toHtml(text))}
    </tr>
  }

  private def makeHeaderRow: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(2)}
      {columnList.map(col => toHtml(col.name))}
    </tr>
  }

  private def makeRow(index: Int): Elem = {

    val pair = pairList(index)

    def colToHtml(col: WLColumn): Elem = {
      toHtml(col.toText(pair.wl, pair.al), col.alignLeft)
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(index + 3)}
      {columnList.map(colToHtml)}
    </tr>
  }

  private def blankRow(index: Int): Elem = {
    <tr>{WLXlsxUtil.makeRowIndex(index)}</tr>
  }

  //noinspection SameParameterValue
  private def ballCbctRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      <td>ballCBCT</td>
    </tr>
  }

  //noinspection SameParameterValue
  private def xyzMmRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toPlainHtml("x (mm)")}
      {toPlainHtml("y (mm)")}
      {toPlainHtml("z (mm)")}
    </tr>
  }

  //noinspection SameParameterValue
  private def zeroRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toPlainHtml("0", alignLeft = false)}
      {toPlainHtml("0", alignLeft = false)}
      {toPlainHtml("0", alignLeft = false)}
    </tr>
  }

  private def zeroDataRow(index: Int, name: String): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toHtml("0", alignLeft = false)}
      {toHtml("0", alignLeft = false)}
      {toHtml("0", alignLeft = false)}
      {toHtml(name)}
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
      zeroDataRow(24, "Ball")
    )
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {WLXlsxUtil.makeAlphaRow(25)}
        {makeTitleRow}
        {makeHeaderRow}
        {pairList.indices.map(makeRow)}
        {makeCbct}
      </table>
    }

    content
  }
}
