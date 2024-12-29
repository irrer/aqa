package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.Util
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLColumn
import org.aqa.webrun.wl.wlMonthly.WLPairDbAl
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessRight

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param pairList WL data
  */
class SSPreprocess(extendedData: ExtendedData, pairList: Seq[WLPairDbAl]) extends SSSheet {

  private def preprocessSorter(a: WLPairDbAl, b: WLPairDbAl): Boolean = {
    val aTable = (360 - a.tableAngle) % 360
    val bTable = (360 - b.tableAngle) % 360

    0 match {
      case _ if aTable > bTable => false
      case _ if aTable < bTable => true

      case _ if a.gantryAngle > b.gantryAngle => false
      case _ if a.gantryAngle < b.gantryAngle => true

      case _ if a.collimatorAngle > b.collimatorAngle => false
      case _ if a.collimatorAngle < b.collimatorAngle => true

      case _ => false
    }
  }

  private val columnIndexList = Seq(0, 1, 2, 4, 5, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

  private val sortedPairList = pairList.sortWith(preprocessSorter)

  override val name: String = "Preprocess"

  val columnList: Seq[WLColumn] = org.aqa.webrun.wl.wlMonthly.WLColumnList(extendedData.machine, pairList.head.acquisition).columnList

  private def toHtml(text: String, alignLeft: Boolean = true): Elem = {
    val c = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    <td class={c}>
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

  private def blankRow(index: Int): Elem = {
    <tr>{WLXlsxUtil.makeRowIndex(index)}</tr>
  }

  private def makeHeaderRow: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(2)}
      {columnIndexList.map(c => toHtml(columnList(c).name))}
    </tr>
  }

  private def makeSortedHeaderRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toHtml("Sorted")}
      {toHtml("")}
      {toHtml("")}
      {sortedColumnIndexList.map(c => toHtml(columnList(c).name))}
    </tr>
  }

  private val sortedColumnIndexList = Seq(4, 5, 3, 6, 7)

  private def makeRow(index: Int): Elem = {

    val pair = pairList(index)

    def colToHtml(col: WLColumn): Elem = {
      toHtml(col.toPreprocessText(pair.wl, pair.al), col.alignLeft)
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(index + 3) :+ columnIndexList.map(c => colToHtml(columnList(c)))}
    </tr>
  }

  private def makeSortedRow(index: Int): Elem = {

    val row = sortedPairList(index)

    def makeCell(c: Int): Elem = {
      val col  = columnList(c)
      toHtml(col.toPreprocessText(row.wl, row.al), col.alignLeft)
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(pairList.size + index + 5)}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {sortedColumnIndexList.map(makeCell)}

    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {WLXlsxUtil.makeAlphaRow(25)}
        {makeTitleRow}
        {makeHeaderRow}
        {pairList.indices.map(makeRow)}
        {blankRow(pairList.size + 3)}
        {makeSortedHeaderRow(pairList.size + 4)}
        {sortedPairList.indices.map(makeSortedRow)}
      </table>
    }

    content
  }
}
