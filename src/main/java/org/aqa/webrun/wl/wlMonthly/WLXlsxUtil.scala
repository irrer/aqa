package org.aqa.webrun.wl.wlMonthly

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.util.CellReference
import org.apache.poi.xssf.streaming.SXSSFCell
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.ExtendedData

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object WLXlsxUtil extends Logging {

  val cssIndex: String = "CSSIndex"
  val cssDataLeft: String = "CSSDataLeft"
  val cssDataRight: String = "CSSDataRight"
  val cssPreprocessLeft: String = "CSSPreprocessLeft"
  val cssPreprocessRight: String = "CSSPreprocessRight"

  def makeRowIndex(index: Int): Elem = {
    <td class={cssIndex}>{index}</td>
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
    * Add a formula cell to the given row.
    * @param row For this row.
    * @param formulaText With this formula.
    */
  def addFormulaCell(row: SXSSFRow, formulaText: String): SXSSFCell = {
    val cellNum: Int = {
      val last = row.getLastCellNum
      if (last < 0) 0 else last
    }
    val cell = row.createCell(cellNum, CellType.FORMULA)
    cell.setCellFormula(formulaText)
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
    * @param columns Number of columns
    * @return HTML for columns showing letters.
    */
  def makeAlphaRow(columns: Int): Elem = {

    def makeAlphaColumn(index: Int): Elem = {
      <td>{CellReference.convertNumToColString(index)}</td>
    }

    // empty cell in upper left corner.
    val blank = { <td> </td> }

    val columnList = blank +: (0 until columns).map(makeAlphaColumn)

    <tr class={cssIndex}>{columnList}</tr>
  }

  /**
    * Round the given angle to the nearest 5 degrees.
    * @param angle Round off this angle.
    * @return Angle rounded off.
    */
  def angleRounded(angle: Double): Int = {
    ((((angle + 720) / 5).round * 5) % 360).toInt
  }

  /**
   * Round a value to 2 significant figures to the right of the decimal point.
   * @param value Round this.
   * @return Rounded value.
   */
  def rnd(value: Double): Double = (value * 100).round / 100.0

  /**
    * Acquisition date+time of file.
    * @param al For this DICOM.
    * @return Acquisition date+time.
    */
  def acq(al: AttributeList): Date = {
    DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get
  }

  /**
    * Make the base for a spreadsheet file name.
    * @param extendedData Metadata.
    * @return Name that caller should add an extension to, such as ".html"
    */
  def baseFileName(extendedData: ExtendedData): String = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'_'HH-mm")
    val dateText = Util.formatDate(dateFormat, extendedData.output.dataDate.get)
    s"WinstonLutz_$dateText"
  }

}
