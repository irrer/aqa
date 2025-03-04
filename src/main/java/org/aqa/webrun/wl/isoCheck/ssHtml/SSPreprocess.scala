package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLColumn
import org.aqa.webrun.wl.isoCheck.WLColumnAlAnonText
import org.aqa.webrun.wl.isoCheck.WLColumnMachine
import org.aqa.webrun.wl.isoCheck.WlColumnWlNumeric
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLMap
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.blankCell
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.blankCells
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.cssPreprocessRight
import org.aqa.webrun.wl.WLRunReq

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param pairList WL data
  */
class SSPreprocess(extendedData: ExtendedData, runReq: WLRunReq, wlMap: WLMap, isoTable: Option[WLIsoTable]) extends SSSheet {

  private def preprocessSorter(a: WinstonLutz, b: WinstonLutz): Boolean = {
    0 match {
      case _ if a.yaw.get > b.yaw.get => false
      case _ if a.yaw.get < b.yaw.get => true

      case _ if a.gantryAngleRounded > b.gantryAngleRounded => false
      case _ if a.gantryAngleRounded < b.gantryAngleRounded => true

      case _ if a.collimatorAngleRounded > b.collimatorAngleRounded => false
      case _ if a.collimatorAngleRounded < b.collimatorAngleRounded => true

      case _ => false
    }
  }

  /** note that the ordering of 4, 5, 3 is intentional.  */
  private val columnIndexList = Seq(0, 1, 2, 4, 5, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

  private val sortedPairList = wlMap.list.sortWith(preprocessSorter)

  override val name: String = "Preprocess"

  val columnList: Seq[WLColumn] = org.aqa.webrun.wl.isoCheck.WLColumnList(extendedData.machine, wlMap.list.head.dataDate).columnList

  private def toHtml(text: String, alignLeft: Boolean = true, isNumeric: Boolean = false, alias: Boolean = false): Elem = {
    val c = if (alignLeft) cssPreprocessLeft else cssPreprocessRight

    val elem =
      if (isNumeric) {
        val elem = {
          <td class={c}>
            {text}
          </td>
        }
        WebUtil.setPrecisionAttr(elem, text.trim.toDouble)
      } else {
        if (alias) {
          <td class={c}>
            {WebUtil.wrapAlias(text)}
          </td>
        } else {
          <td class={c}>
            {text}
          </td>
        }
      }

    elem
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

  private def makeSortedHeaderRow(): Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(19)}
      {toHtml("Sorted")}
      {toHtml("")}
      {toHtml("")}
      {sortedColumnIndexList.map(c => toHtml(columnList(c).name))}
      {blankCells(18)}
    </tr>
  }

  private def makeFillerRows(): Seq[Elem] = {
    0 match {
      case _ if isoTable.isEmpty                                => (12 to 17).map(blankRow) // no table
      case _ if isoTable.isDefined && isoTable.get.T_30.isEmpty => (14 to 17).map(blankRow) // has table, but only 2 table beams
      case _                                                    => Seq() //                          has table, with 6 table beams
    }
  }

  private val sortedColumnIndexList = Seq(4, 5, 3, 6, 7)

  def colToHtml(wl: WinstonLutz, col: WLColumn): Elem = {
    toHtml(
      col.toPreprocessText(wl, runReq.alOf(wl).get),
      col.alignLeft,
      isNumeric = col.isInstanceOf[WlColumnWlNumeric],
      alias = col.isInstanceOf[WLColumnAlAnonText] || col.isInstanceOf[WLColumnMachine]
    )
  }

  private def makeRow(index: Int): Elem = {

    val wl = wlMap.list(index)

    <tr>
      {WLXlsxUtil.makeRowIndex(index + 3) :+ columnIndexList.map(c => colToHtml(wl, columnList(c)))}
      {blankCell}
    </tr>
  }

  private def makeSortedRow(index: Int): Elem = {

    val wl = sortedPairList(index)

    def makeCell(c: Int): Elem = {
      val col = columnList(c)
      toHtml(
        col.toPreprocessText(wl, runReq.alOf(wl).get),
        col.alignLeft,
        isNumeric = col.isInstanceOf[WlColumnWlNumeric],
        alias = col.isInstanceOf[WLColumnAlAnonText] || col.isInstanceOf[WLColumnMachine]
      )
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(wlMap.list.size + index + 11)}
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
        {wlMap.list.indices.map(makeRow)}
        {makeFillerRows()}
        {blankRow(18)}
        {makeSortedHeaderRow()}
        {sortedPairList.indices.map(makeSortedRow)}
        {blankRow(lastRowIndex)}
      </table>
    }

    content
  }
}
