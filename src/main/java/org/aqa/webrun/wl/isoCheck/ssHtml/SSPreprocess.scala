package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLColumn
import org.aqa.webrun.wl.isoCheck.WlColumnWlNumeric
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.blankCell
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.blankCells
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssPreprocessRight
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.flip

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param pairList WL data
  */
class SSPreprocess(extendedData: ExtendedData, pairList: Seq[WLBeam], isoTable: Option[WLIsoTable]) extends SSSheet {

  private def preprocessSorter(a: WLBeam, b: WLBeam): Boolean = {
    val aIsoTable = flip(a.isoTableAngle)
    val bIsoTable = flip(b.isoTableAngle)

    0 match {
      case _ if aIsoTable > bIsoTable => false
      case _ if aIsoTable < bIsoTable => true

      case _ if a.gantryAngle > b.gantryAngle => false
      case _ if a.gantryAngle < b.gantryAngle => true

      case _ if a.collimatorAngle > b.collimatorAngle => false
      case _ if a.collimatorAngle < b.collimatorAngle => true

      case _ => false
    }
  }

  /** note that the ordering of 4, 5, 3 is intentional.  */
  private val columnIndexList = Seq(0, 1, 2, 4, 5, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

  private val sortedPairList = pairList.sortWith(preprocessSorter)

  override val name: String = "Preprocess"

  val columnList: Seq[WLColumn] = org.aqa.webrun.wl.isoCheck.WLColumnList(extendedData.machine, pairList.head.acquisition).columnList

  private def toHtml(text: String, alignLeft: Boolean = true, isNumeric: Boolean = false): Elem = {
    val c = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    if (isNumeric) {
      val elem = {
        <td class={c}>
          {text}
        </td>
      }
      WebUtil.setPrecisionAttr(elem, text.trim.toDouble)
    } else {
      <td class={c}>
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
      {blankCells(23)}
    </tr>
  }

  private def blankRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {blankCells(26)}
    </tr>
  }

  private def makeHeaderRow: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(2)}
      {columnIndexList.map(c => toHtml(columnList(c).name))}
      {blankCell}
    </tr>
  }

  private def makeSortedHeaderRow(index: Int): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(index)}
      {toHtml("Sorted")}
      {toHtml("")}
      {toHtml("")}
      {sortedColumnIndexList.map(c => toHtml(columnList(c).name))}
      {blankCells(18)}
    </tr>
  }

  private val sortedColumnIndexList = Seq(4, 5, 3, 6, 7)

  def colToHtml(pair: WLBeam, col: WLColumn): Elem = {
    toHtml(col.toPreprocessText(pair.wl, pair.al), col.alignLeft, isNumeric = col.isInstanceOf[WlColumnWlNumeric])
  }

  private def makeRow(index: Int): Elem = {

    val pair = pairList(index)

    <tr>
      {WLXlsxUtil.makeRowIndex(index + 3) :+ columnIndexList.map(c => colToHtml(pair, columnList(c)))}
      {blankCell}
    </tr>
  }

  private def makeSortedRow(index: Int): Elem = {

    val row = sortedPairList(index)

    def makeCell(c: Int): Elem = {
      val col = columnList(c)
      toHtml(col.toPreprocessText(row.wl, row.al), col.alignLeft, isNumeric = col.isInstanceOf[WlColumnWlNumeric])
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(pairList.size + index + 5)}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {sortedColumnIndexList.map(makeCell)}
      {blankCells(18)}
    </tr>
  }

  override def make(): Elem = {

    val lastRowIndex = if (isoTable.isDefined) 35 else 23

    val content = {
      <table class="table table-bordered">
        {WLXlsxUtil.makeAlphaRow(26)}
        {makeTitleRow}
        {makeHeaderRow}
        {pairList.indices.map(makeRow)}
        {blankRow(pairList.size + 3)}
        {makeSortedHeaderRow(pairList.size + 4)}
        {sortedPairList.indices.map(makeSortedRow)}
        {blankRow(lastRowIndex)}
      </table>
    }

    content
  }
}
