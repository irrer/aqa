package org.aqa.webrun.wl.wlMonthly

import org.apache.poi.ss.usermodel.BorderStyle
import org.apache.poi.ss.usermodel.CellStyle
import org.apache.poi.ss.usermodel.FillPatternType
import org.apache.poi.ss.usermodel.HorizontalAlignment
import org.apache.poi.ss.usermodel.IndexedColors
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.addNumericCell
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.addStringCell
import org.aqa.Logging
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssDataRight
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessRight

import scala.xml.Elem

/**
  * Make the CBCT portion of the Data worksheet.
  * @param sheet For this sheet.
  * @param dataStyle Style of data elements.
  * @param columnListSize Total number of columns.
  */
class WLDataCBCT(sheet: SXSSFSheet, dataStyle: CellStyle, columnListSize: Int) extends Logging {

  private val plainStyle: CellStyle = {

    val style: CellStyle = sheet.getWorkbook.createCellStyle()

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

  private def fillRemainingCells(row: SXSSFRow): Unit = {
    (row.getLastCellNum until columnListSize).foreach(_ => addStringCell(row, "").setCellStyle(plainStyle))
  }

  private def fillRemainingElem(currentSize: Int): Seq[Elem] = (currentSize until columnListSize).map(_ => <td></td>)

  private def addBlankRow(): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    addStringCell(row, "").setCellStyle(plainStyle)
    fillRemainingCells(row)
    <tr>
      {WLXlsxUtil.makeRowIndex(row)}
      {fillRemainingElem(0)}
    </tr>
  }

  private def addBallCBCT(): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    addStringCell(row, "ballCBCT").setCellStyle(plainStyle)
    fillRemainingCells(row)
    <tr>
      {WLXlsxUtil.makeRowIndex(row)}
      <td class={cssPreprocessLeft}>ballCBCT</td>
      {fillRemainingElem(1)}
    </tr>
  }

  private def addHeader(): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    addStringCell(row, "x (mm)").setCellStyle(plainStyle)
    addStringCell(row, "y (mm)").setCellStyle(plainStyle)
    addStringCell(row, "y (mm)").setCellStyle(plainStyle)
    fillRemainingCells(row)
    <tr>
      {WLXlsxUtil.makeRowIndex(row)}
      <td class={cssPreprocessLeft}>x (mm)</td>
      <td class={cssPreprocessLeft}>y (mm)</td>
      <td class={cssPreprocessLeft}>z (mm)</td>
      {fillRemainingElem(3)}
    </tr>
  }

  private def addZeroRow(): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    (0 until 3).foreach(_ => addNumericCell(row, 0.0).setCellStyle(plainStyle))
    fillRemainingCells(row)
    <tr>
      {WLXlsxUtil.makeRowIndex(row)}
      <td class={cssPreprocessRight}>0</td>
      <td class={cssPreprocessRight}>0</td>
      <td class={cssPreprocessRight}>0</td>
      {fillRemainingElem(3)}
    </tr>
  }

  private def addAcqIsocenter(): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    (0 until 3).foreach(_ => addNumericCell(row, 0.0).setCellStyle(dataStyle))
    addStringCell(row, "AcqIsocenter").setCellStyle(plainStyle)
    fillRemainingCells(row)
    <tr>
      {WLXlsxUtil.makeRowIndex(row)}
      <td class={cssDataRight}>0</td>
      <td class={cssDataRight}>0</td>
      <td class={cssDataRight}>0</td>
      <td>AcqIsocenter</td>
      {fillRemainingElem(4)}
    </tr>
  }

  private def addBall(): Elem = {
    val row = sheet.createRow(sheet.getLastRowNum + 1)
    (0 until 3).foreach(_ => addNumericCell(row, 0.0).setCellStyle(dataStyle))
    addStringCell(row, "Ball").setCellStyle(plainStyle)
    fillRemainingCells(row)
    (0 until 5).foreach(_ => addBlankRow())
    <tr>
      {WLXlsxUtil.makeRowIndex(row)}
      <td class={cssDataRight}>0</td>
      <td class={cssDataRight}>0</td>
      <td class={cssDataRight}>0</td>
      <td>Ball</td>
    </tr>
  }

  def make(): Seq[Elem] = {
    val elem: Seq[Elem] = Seq(
      addBlankRow(),
      addBallCBCT(),
      addHeader(),
      addZeroRow(),
      addBlankRow(),
      addAcqIsocenter(),
      addBall()
    )

    elem
  }

}
