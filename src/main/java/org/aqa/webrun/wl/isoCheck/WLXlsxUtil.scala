package org.aqa.webrun.wl.isoCheck

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.util.CellReference
import org.apache.poi.xssf.streaming.SXSSFCell
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet
import org.aqa.Logging
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object WLXlsxUtil extends Logging {

  private val cssIndex: String = "CSSIndex"
  val cssDataLeft: String = "CSSDataLeft"
  val cssDataRight: String = "CSSDataRight"
  val cssPreprocessLeft: String = "CSSPreprocessLeft"
  val cssPreprocessRight: String = "CSSPreprocessRight"

  val numericFormat = "%24.20f"

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
  def rnd(value: Double): Double = value // (value * 100).round / 100.0  // shut off rounding

  private def fmt(d: Double): String = d.formatted(numericFormat).trim

  def toHtml(text: String, alignLeft: Boolean = true, style: Option[String] = None): Elem = {
    val c = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    val s = if (style.isDefined) style.get else ""

    <td class={c} style={s}>
      {text}
    </td>
  }

  def toHtmlPeach(dbl: Double): Elem = {
    val elem = <td class={cssPreprocessRight} style="border:2px solid black;background:#F8CBAD;">
      {fmt(dbl)}
    </td>

    WebUtil.setPrecisionAttr(elem, dbl)
  }

  def toHtmlPowderBlue(text: String): Elem = {
    <td class={cssPreprocessLeft} style="border:2px solid black;background:#DDEBF7;">
      {text}
    </td>
  }

  def toHtmlPowderBlue(dbl: Double): Elem = {
    val elem = {
      <td class={cssPreprocessRight} style="border:2px solid black;background:#DDEBF7;">
      {fmt(dbl)}
    </td>
    }
    WebUtil.setPrecisionAttr(elem, dbl)
  }

  def toHtmlYellow(dbl: Double, style: Option[String] = None): Elem = {
    val s = {
      val yellow = "background:#FFFF00;"
      if (style.isDefined)
        yellow + style.get
      else
        yellow
    }

    val elem = {
      <td class={cssPreprocessRight} style={s}>
        {fmt(dbl)}
      </td>
    }
    WebUtil.setPrecisionAttr(elem, dbl)
  }

  def toHtmlYellowText(text: String, style: Option[String] = None): Elem = {

    val s = {
      val yellow = "background:#FFFF00;"
      if (style.isDefined)
        yellow + style.get
      else
        yellow
    }

    val elem = {
      <td class={cssPreprocessRight} style={s}>
        {text}
      </td>
    }
    elem
  }

  def toHtml(dbl: Double): Elem = {
    val elem = {
      <td class={cssPreprocessRight}>
      {dbl.toString}
    </td>
    }

    WebUtil.setPrecisionAttr(elem, dbl)
  }

  def toHtml(dbl: Option[Double]): Elem = {
    if (dbl.isDefined)
      toHtml(dbl.get)
    else
      toHtml("")
  }

  def toHtml(int: Int): Elem = {
    toHtml(int.toString)
  }

  def blankCells(count: Int): Seq[Elem] = {
    (0 until count).map(_ => toHtml(""))
  }

  def blankCell: Elem = {
    blankCells(1).head
  }

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
    val machineName = FileUtil.replaceInvalidFileNameCharacters(extendedData.machine.getRealId, '_')
    val dateText = Util.formatDate(dateFormat, extendedData.output.dataDate.get)
    s"IsoCheck_${machineName}_$dateText"
  }

}
