package org.aqa.webrun.wl.wlXlsx

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.apache.poi.ss.usermodel.BorderStyle
import org.apache.poi.ss.usermodel.CellStyle
import org.apache.poi.ss.usermodel.FillPatternType
import org.apache.poi.ss.usermodel.HorizontalAlignment
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet
import org.apache.poi.xssf.streaming.SXSSFWorkbook
import org.aqa.db.WinstonLutz
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLRunReq
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.wl.wlXlsx.WLXlsxUtil.addStringCell
import org.aqa.webrun.wl.wlXlsx.WLXlsxUtil.cssData
import org.aqa.webrun.wl.wlXlsx.WLXlsxUtil.makeAlphaRow
import org.aqa.webrun.wl.wlXlsx.WLXlsxUtil.makeRowIndex
import org.aqa.AnonymizeUtil
import org.aqa.webrun.wl.wlXlsx.WLXlsxUtil.addNumericCell
import org.aqa.Logging
import org.aqa.webrun.wl.wlXlsx.WLXlsxUtil.getLastColumnNumber

import java.util.Date
import scala.xml.Elem

case class WLData(extendedData: ExtendedData, runReq: WLRunReq, dbList: Seq[WinstonLutz], workbook: SXSSFWorkbook) extends WLSheetMaker with Logging {

  private def acquistion(al: AttributeList): Date = DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get

  // get the date+time of the first slice
  private val firstDateTime: Date = runReq.epidList.map(acquistion).min

  private def fieldNameOf(al: AttributeList): String = {
    val gantry = Util.angleRoundedTo90(Util.gantryAngle(al)).formatted("%03d")
    val collimator = Util.angleRoundedTo90(Util.collimatorAngle(al)).formatted("%03d")

    val elapsed_ms = acquistion(al).getTime - firstDateTime.getTime

    val minute = (" " + ((elapsed_ms / (60 * 1000)) % 60).formatted("%d")).takeRight(2)
    val second = ((elapsed_ms / 1000) % 60).formatted("%02d")

    val fn = s"G$gantry C$collimator $minute:$second"
    fn
  }

  override val sheetName: String = "Data"

  private val dataStyle: CellStyle = {

    val style: CellStyle = workbook.createCellStyle()

    val backgroundColor = {
      val d = 217.toByte
      val bc = new org.apache.poi.xssf.usermodel.XSSFColor(Array[Byte](d, d, d))
      bc
    }

    style.setFillBackgroundColor(backgroundColor)
    style.setFillForegroundColor(backgroundColor)
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND)

    style.setBorderBottom(BorderStyle.THIN)
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderLeft(BorderStyle.THIN)
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderRight(BorderStyle.THIN)
    style.setRightBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderTop(BorderStyle.THIN)
    style.setTopBorderColor(IndexedColors.BLACK.getIndex)

    style.setAlignment(HorizontalAlignment.RIGHT)

    // style.setIndention(1.toShort)

    style
  }

  private val plainStyle: CellStyle = {

    val style: CellStyle = workbook.createCellStyle()

    style.setFillBackgroundColor(IndexedColors.WHITE.getIndex)
    style.setFillForegroundColor(IndexedColors.WHITE.getIndex)
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND)

    style.setBorderBottom(BorderStyle.THIN)
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderLeft(BorderStyle.THIN)
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderRight(BorderStyle.THIN)
    style.setRightBorderColor(IndexedColors.BLACK.getIndex)
    style.setBorderTop(BorderStyle.THIN)
    style.setTopBorderColor(IndexedColors.BLACK.getIndex)

    style.setAlignment(HorizontalAlignment.LEFT)

    style
  }

  private def addTitleRow(sheet: SXSSFSheet): Elem = {
    val row: SXSSFRow = sheet.createRow(sheet.getLastRowNum + 1)

    addStringCell(row, "Winston-Lutz Field Data")

    val dataDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)
    addStringCell(row, s"Data Date: $dataDateText")

    val analysisDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.analysisDate.get)
    addStringCell(row, s"Analysis Date: $analysisDateText")

    def toXml(text: String): Elem = <td class={cssData.name}>
      {text}
    </td>

    val elemList: Seq[Elem] = WLXlsxUtil.makeRowIndex(row) +: (0 until row.getLastCellNum).map(row.getCell).map(_.getStringCellValue).map(toXml)
    <tr>
      {elemList}
    </tr>
  }

  private abstract case class Col( //
      name: String
  ) {
    def getName: String = name

    def toText(wl: WinstonLutz, al: AttributeList): String

    def addCell(row: SXSSFRow, wl: WinstonLutz, al: AttributeList): Unit
  }

  private class ColText(name: String, toVal: (WinstonLutz, AttributeList) => String) extends Col(name) {
    override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(wl, al)

    override def addCell(row: SXSSFRow, wl: WinstonLutz, al: AttributeList): Unit = addStringCell(row, toText(wl, al))
  }

  private class ColAlDbl(name: String, tag: AttributeTag) extends Col(name) {
    private def toVal(al: AttributeList): Double = DicomUtil.findAllSingle(al, tag).head.getDoubleValues.head

    override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(al).toString

    override def addCell(row: SXSSFRow, wl: WinstonLutz, al: AttributeList): Unit = addNumericCell(row, toVal(al))
  }

  private class ColWlDbl(name: String, toVal: WinstonLutz => Double) extends Col(name) {
    override def toText(wl: WinstonLutz, al: AttributeList): String = toVal(wl).toString

    override def addCell(row: SXSSFRow, wl: WinstonLutz, al: AttributeList): Unit = addNumericCell(row, toVal(wl))
  }

  private class ColAlAnonText(name: String, tag: AttributeTag) extends Col(name) {

    override def toText(wl: WinstonLutz, al: AttributeList): String = {
      val text = AnonymizeUtil.deAnonymizeAttribute(extendedData.institution.institutionPK.get, al.get(tag)) match {
        case Some(attr) => attr.getSingleStringValueOrEmptyString
        case _          => "NA"
      }
      text
    }

    override def addCell(row: SXSSFRow, wl: WinstonLutz, al: AttributeList): Unit = {
      addStringCell(row, toText(wl, al))
    }
  }

  private val columnList: Seq[Col] = {
    val list: Seq[Col] = Seq(
      new ColText("machine id", (_: WinstonLutz, _: AttributeList) => extendedData.machine.getRealId),
      new ColText("field name", (_: WinstonLutz, al: AttributeList) => fieldNameOf(al)),
      new ColText(
        "status",
        (wl: WinstonLutz, _: AttributeList) => {
          val limit = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get).passLimit_mm
          if (wl.errorXY_mm < limit) "Passed" else "Failed"
        }
      ),
      new ColAlDbl("table yaw", TagByName.PatientSupportAngle),
      // @formatter:off
      new ColWlDbl("gantry angle",                    _.gantryAngle_deg),
      new ColWlDbl("coll angle",                      _.collimatorAngle_deg),
      new ColWlDbl("X offset corrected box-ball",     _.errorX_mm),
      new ColWlDbl("Y offset corrected box-ball",     _.errorY_mm),
      new ColWlDbl("XY offset corrected",             _.errorXY_mm),
      new ColWlDbl("X box center corrected",          _.boxCenterX_mm),
      new ColWlDbl("Y box center corrected",          _.boxCenterY_mm),
      new ColWlDbl("X tongue and groove correction",  _ => 0.0),
      new ColWlDbl("Y tongue and groove correction",  _ => 0.0),
      new ColWlDbl("X ball center",                   _.ballX_mm),
      new ColWlDbl("Y ball center",                   _.ballY_mm),
      new ColWlDbl("box left uncorrected",            _.leftEdge_mm),
      new ColWlDbl("box right uncorrected",           _.rightEdge_mm),
      new ColWlDbl("box top uncorrected",             _.topEdge_mm),
      new ColWlDbl("box bottom uncorrected",          _.bottomEdge_mm),
      new ColWlDbl("X box center uncorrected",        _.boxCenterX_mm),
      new ColWlDbl("Y box center uncorrected",        _.boxCenterY_mm),
      new ColAlAnonText("Patient ID",            TagByName.PatientID),
      new ColAlAnonText("Patient Name",          TagByName.PatientName),
      new ColAlAnonText("Instance (slice) UID",  TagByName.SOPInstanceUID),
      new ColAlAnonText("Series UID",            TagByName.SeriesInstanceUID)
      // @formatter:on
    )

    list
  }

  /**
   * Add the column headers.
   *
   * @param sheet For this sheet.
   */
  private def addHeaderRow(sheet: SXSSFSheet): Elem = {

    val row = sheet.createRow(sheet.getLastRowNum + 1)

    columnList.foreach(col => addStringCell(row, col.getName))

    <tr>
      {makeRowIndex(row) +: columnList.map(col => <td class={cssData.name}>
      {col.getName}
    </td>)}
    </tr>

  }

  private def addContentRow(sheet: SXSSFSheet, wl: WinstonLutz, al: AttributeList): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)

    def makeElem(col: Col): Elem = {
      col.addCell(row, wl, al)
      val text = col.toText(wl, al)
      <td class={cssData.name}>
        {text}
      </td>
    }

    val elemRow = {
      <tr>
        {makeRowIndex(row) +: columnList.map(makeElem)}
      </tr>
    }

    elemRow
  }

  private def addContentRowList(sheet: SXSSFSheet): Seq[Elem] = {
    def acquisitionDateTime(al: AttributeList): Date = {
      DicomUtil.getTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime).get
    }

    val alList = runReq.epidList.sortBy(acquisitionDateTime)

    def wlOfAl(al: AttributeList): Option[WinstonLutz] = {
      val alSop = Util.sopOfAl(al)
      dbList.find(wl => wl.rtimageUID.equals(alSop))
    }

    val wlList = alList.map(al => (wlOfAl(al), al)).filter(_._1.isDefined)

    val elemList = wlList.map(wlAl => addContentRow(sheet, wlAl._1.get, wlAl._2))

    elemList
  }

  private def makeCbct(sheet: SXSSFSheet): Unit = {
    def fillRemainingCells(row: SXSSFRow): Unit = {
      (row.getLastCellNum until columnList.size).foreach(_ => addStringCell(row, "").setCellStyle(plainStyle))
    }

    def addBlankRow(): Unit = {
      val row = sheet.createRow(sheet.getLastRowNum + 1)
      addStringCell(row, "").setCellStyle(plainStyle)
      fillRemainingCells(row)
    }

    addBlankRow()

    val rowBallCBCT = sheet.createRow(sheet.getLastRowNum + 1)
    addStringCell(rowBallCBCT, "ballCBCT").setCellStyle(plainStyle)
    fillRemainingCells(rowBallCBCT)

    val rowHeader = sheet.createRow(sheet.getLastRowNum + 1)
    addStringCell(rowHeader, "x (mm)").setCellStyle(plainStyle)
    addStringCell(rowHeader, "y (mm)").setCellStyle(plainStyle)
    addStringCell(rowHeader, "y (mm)").setCellStyle(plainStyle)
    fillRemainingCells(rowHeader)

    val rowZero = sheet.createRow(sheet.getLastRowNum + 1)
    (0 until 3).foreach(_ => addNumericCell(rowZero, 0.0).setCellStyle(plainStyle))
    fillRemainingCells(rowZero)

    addBlankRow()

    val rowAcql = sheet.createRow(sheet.getLastRowNum + 1)
    (0 until 3).foreach(_ => addNumericCell(rowAcql, 0.0).setCellStyle(dataStyle))
    addStringCell(rowAcql, "AcqIsocenter").setCellStyle(plainStyle)
    fillRemainingCells(rowAcql)

    val rowBall = sheet.createRow(sheet.getLastRowNum + 1)
    (0 until 3).foreach(_ => addNumericCell(rowBall, 0.0).setCellStyle(dataStyle))
    addStringCell(rowBall, "Ball").setCellStyle(plainStyle)
    fillRemainingCells(rowBall)
    (0 until 5).foreach(_ => addBlankRow())

    val elem: Seq[Elem] = Seq(
      {
        <tr>
          <td></td>
        </tr>
      },
      {
        <tr>
          <td>ballCBCT</td>
        </tr>
      }
    )

    // TODO  add Elem
  }

  override def make(): Elem = {
    val sheet: SXSSFSheet = workbook.createSheet(sheetName)

    // turn on auto sizing for all columns
    (0 to (columnList.size + 1)).foreach(i => sheet.trackColumnForAutoSizing(i))

    val rowList =
      Seq(addTitleRow(sheet), addHeaderRow(sheet)) ++
        addContentRowList(sheet) ++
        new WLDataCBCT(sheet, dataStyle, columnList.size).make()

    val indexRow = makeAlphaRow(sheet)
    // fill the remainder of the top row with empty cells.  This is required to make auto-sizing work.
    val lastColumnNumber = getLastColumnNumber(sheet)
    val topRow = sheet.getRow(0)
    (topRow.getLastCellNum until lastColumnNumber).foreach(i => topRow.createCell(i).setCellValue(""))

    (0 until getLastColumnNumber(sheet)).foreach(i => {
      sheet.setDefaultColumnStyle(i, dataStyle)
      Trace.trace(i)
      sheet.autoSizeColumn(i)
    })

    val elem = {
      <table class="table table-bordered">
        {indexRow}{rowList}
      </table>
    }

    elem
  }
}

object WLData {

  // Replace with your Excel file path and the desired HTML file path
  private val excelFilePath = """D:\tmp\wl\2024_WinstonLutz_TB5_2024-10-15.xls"""
  private val htmlFilePath = """D:\tmp\wl\html\WL.html"""

  def main(args: Array[String]): Unit = {
    Trace.trace()

    Trace.trace()
  }
}
