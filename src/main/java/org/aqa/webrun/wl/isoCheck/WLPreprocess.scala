package org.aqa.webrun.wl.isoCheck

import org.apache.poi.ss.util.CellReference
import org.apache.poi.xssf.streaming.SXSSFCell
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet
import org.apache.poi.xssf.streaming.SXSSFWorkbook
import org.aqa.Logging
import org.aqa.db.WinstonLutz
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.addFormulaCell
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.addStringCell
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.makeRowIndex
import org.aqa.Util

import scala.xml.Elem

case class WLPreprocess(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz], workbook: SXSSFWorkbook) extends WLSheetMaker with Logging {

  private val dataSheet = workbook.getSheetAt(0) // TODO change when SNCImport sheet is added.

  override val sheetName: String = "Preprocess"

  val sheet: SXSSFSheet = workbook.createSheet(sheetName)

  private val columnListSize = 22

  private def fillRemainingCells(row: SXSSFRow): Unit = {
    (row.getLastCellNum until columnListSize).foreach(_ => addStringCell(row, ""))
  }

  //noinspection SameParameterValue
  private def fillRemainingElem(currentSize: Int): Seq[Elem] = (currentSize until columnListSize).map(_ => <td></td>)

  private def copyTitle(): Elem = {
    val row: SXSSFRow = sheet.createRow(sheet.getLastRowNum + 1)

    addFormulaCell(row, "Data!A1")
    addFormulaCell(row, "Data!B1")
    addFormulaCell(row, "Data!C1")
    fillRemainingCells(row)

    val dataRow = dataSheet.getRow(0)

    val elem = {
      <tr>
        {makeRowIndex(row)}
        <td>{dataRow.getCell(0).getStringCellValue}</td>
        <td>{dataRow.getCell(1).getStringCellValue}</td>
        <td>{dataRow.getCell(2).getStringCellValue}</td>
        {fillRemainingElem(3)}
      </tr>
    }

    (0 until 3).foreach(i => sheet.setColumnWidth(i, dataSheet.getColumnWidth(i)))

    elem
  }

  /** First row as it appears in Excel (1-relative).  This is the row with the column headers. */
  private val firstRow = 3

  /** Last row as it appears in Excel (1-relative). */
  private val lastRow = 17

  private abstract class Col(index: String) {
    val intIndex: Int = CellReference.convertColStringToIndex(index)
    def addCell(row: SXSSFRow): Elem
    protected def getDataCell(row: SXSSFRow): SXSSFCell =
      dataSheet.getRow(row.getRowNum + 1).getCell(intIndex)
  }

  private class ColString(val index: String) extends Col(index) {
    override def addCell(row: SXSSFRow): Elem = {
      val loc = s"$index${row.getRowNum + 1}"
      val formula = s"Data!$loc"
      addFormulaCell(row, formula)
      <td>{getDataCell(row).getStringCellValue}</td>
    }
  }

  private class ColAngle(val index: String) extends Col(index) {
    override def addCell(row: SXSSFRow): Elem = {
      val loc = s"$index${row.getRowNum + 1}"
      val formula = s"IF(ROUND(Data!$loc,0)=360,0,ROUND(Data!$loc,0))"
      addFormulaCell(row, formula)
      <td>{Util.angleRoundedTo90(getDataCell(row).getNumericCellValue).toString}</td>
    }
  }

  private class ColNumeric(val index: String) extends Col(index) {
    override def addCell(row: SXSSFRow): Elem = {
      val loc = s"$index${row.getRowNum + 1}"
      val formula = s"ROUND(Data!$loc,2)"
      addFormulaCell(row, formula)
      <td>{getDataCell(row).getNumericCellValue.formatted("%6.2f").trim}</td>
    }
  }

  private val mainIsoTableColumnList: Seq[Col] = Seq(
    new ColString("A"),
    new ColString("B"),
    new ColString("C"),
    new ColAngle("E"),
    new ColAngle("F"),
    new ColAngle("D"),
    new ColNumeric("G"),
    new ColNumeric("H"),
    new ColNumeric("I"),
    new ColNumeric("J"),
    new ColNumeric("K"),
    new ColNumeric("L"),
    new ColNumeric("M"),
    new ColNumeric("N"),
    new ColNumeric("O"),
    new ColNumeric("P"),
    new ColNumeric("Q"),
    new ColNumeric("R"),
    new ColNumeric("S"),
    new ColNumeric("T"),
    new ColNumeric("U"),
    new ColString("V")
  )

  private def mainIsoTableHeaders(): Elem = {

    val row: SXSSFRow = sheet.createRow(sheet.getLastRowNum + 1)

    def addColHeader(col: Col): Elem = {
      val formula = s"Data!${CellReference.convertNumToColString(col.intIndex)}${firstRow - 1}"
      WLXlsxUtil.addFormulaCell(row, formula)
      // WLXlsxUtil.addStringCell(row, formula)

      val name = dataSheet.getRow(1).getCell(col.intIndex).getStringCellValue
      <td>{name}</td>
    }

    <tr>
      {WLXlsxUtil.makeRowIndex(firstRow - 1)}
      {mainIsoTableColumnList.map(addColHeader)}
    </tr>
  }

  private def makeMainIsoTableRow(i: Int): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    val list = mainIsoTableColumnList.map(col => col.addCell(row))
    <tr>
      {WLXlsxUtil.makeRowIndex(i - 1)}
      {list}
    </tr>
  }

  private def mainIsoTable(): Seq[Elem] = {

    val rowList = (firstRow + 1 to lastRow).map(i => makeMainIsoTableRow(i))

    rowList
  }

  override def make(): Elem = {
    // turn on auto sizing for all columns
    // (0 to (columnListSize + 1)).foreach(i => sheet.trackColumnForAutoSizing(i))
    // (0 until columnListSize).foreach(i => sheet.autoSizeColumn(i))

    val elem = {
      <table class="table table-bordered">
        {WLXlsxUtil.makeAlphaRow(25)}
        {copyTitle()}
        {mainIsoTableHeaders()}
        {mainIsoTable()}
      </table>
    }

    elem
  }
}
