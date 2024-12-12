package org.aqa.webrun.wl.wlXlsx

import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.util.CellReference
import org.apache.poi.xssf.streaming.SXSSFCell
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet

import scala.xml.Elem

object WLXlsxUtil {

  case class CSS(name: String, style: String) {
    def toCss: String = "." + name + "{" + style + "}"

    override def toString: String = name
  }

  val cssIndex = CSS("CSSIndex", "text-align:center;background-color:#f0f0f0;color:black;font-size:12pt;")
  val cssData = CSS("CSSData", "text-align:left;background-color:#d9d9d9;color:black;font-size:12pt;")

  val cssStyle = {
    Seq(cssIndex, cssData)
      .map(_.toCss)
      .mkString("\n")
  }

  def makeRowIndex(index: Int): Elem = {
    <td class={cssIndex.name}>{index}</td>
  }

  /**
    * Make the row HTML that shows the row number and is used to prefix the content.
    * @param row For this row.
    * @return A single cell.
    */
  def makeRowIndex(row: SXSSFRow): Elem = {
    makeRowIndex(row.getRowNum + 1)
  }

  /**
    * Add a text cell to the given row.
    *
    * @param row  For this row.
    * @param text With this content.
    */
  def addStringCell(row: SXSSFRow, text: String): SXSSFCell = {
    val cellNum: Int = {
      val last = row.getLastCellNum
      if (last < 0) 0 else last
    }
    val cell = row.createCell(cellNum, CellType.STRING)
    cell.setCellValue(text)
    cell
  }

  /**
    * Add a numeric cell to the given row.
    * @param row For this row.
    * @param value With this content.
    */
  def addNumericCell(row: SXSSFRow, value: Double): SXSSFCell = {
    val cellNum: Int = {
      val last = row.getLastCellNum
      if (last < 0) 0 else last
    }
    val cell = row.createCell(cellNum, CellType.NUMERIC)
    cell.setCellValue(value)
    cell
  }

  /**
   * Get the index of the rightmost column in the sheet.
   * @param sheet For this sheet.
   * @return Max column index.
   */
  def getLastColumnNumber(sheet: SXSSFSheet): Int = {
    val max = (0 to sheet.getLastRowNum).map(rowIndex => sheet.getRow(rowIndex).getLastCellNum).max
    max.toInt
  }

  /**
    * Make the header that shows the letters of the columns.  Make it as wide as the widest row.
    * @param sheet For this sheet.
    * @return HTML for columns showing letters.
    */
  def makeAlphaRow(sheet: SXSSFSheet): Elem = {

    def makeAlphaColumn(index: Int): Elem = {
      <td>{CellReference.convertNumToColString(index)}</td>
    }

    // empty cell in upper left corner.
    val blank = { <td> </td> }

    val columnList = blank +: (0 until getLastColumnNumber(sheet)).map(makeAlphaColumn)

    <tr class={cssIndex.name}>{columnList}</tr>
  }

}
