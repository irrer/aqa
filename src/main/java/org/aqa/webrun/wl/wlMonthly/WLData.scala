package org.aqa.webrun.wl.wlMonthly

import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLRunReq
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import java.io.File
import java.io.FileOutputStream
import java.util.Date

object WLData extends Logging {

  /** First row of Data sheet content (inclusive). */
  private val firstRowNum = 3

  /** Last row of Data sheet content (inclusive). */
  private val lastRowNum = 17

  def makeSpreadsheet(extendedData: ExtendedData, runReq: WLRunReq, pairList: Seq[WLPairDbAl]): String = {

    val workbook = new XSSFWorkbook(Config.WLMonthlyTemplateFile)

    // get the date+time of the first slice
    val firstDateTime: Date = pairList.head.acquisition

    val columnList = WLColumnList(extendedData.machine, firstDateTime).columnList

    // the Data worksheet
    val sheet: XSSFSheet = workbook.getSheetAt(1)

    def updateTitleRow(): Unit = {
      val row = sheet.getRow(0)

      val dataDateCell = row.getCell(1)
      val dataDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)
      dataDateCell.setCellValue(s"Data Date: $dataDateText")

      val analysisDateCell = row.getCell(2)
      val analysisDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.analysisDate.get)
      analysisDateCell.setCellValue(s"Data Date: $analysisDateText")
    }

    def updateContentRow(rowNum: Int, pair: WLPairDbAl): Unit = {
      val row = sheet.getRow(rowNum - 1) // subtracting 1 converts a row number to a 0-relative row index

      columnList.indices.foreach(i => columnList(i).updateCell(row.getCell(i), pair.wl, pair.al))
    }

    /**
      * Update all the content rows.  Sort by acquisition time, associate each data set with a row, and then process each row.
      */
    def updateContentRowList(): Unit = {
      (firstRowNum until lastRowNum).foreach(rowNum => updateContentRow(rowNum, pairList(rowNum - firstRowNum)))
    }

    def update(): Unit = {
      // turn on auto sizing for all columns
      updateTitleRow()
      updateContentRowList()
    }

    update()

    val file = new File(extendedData.output.dir, WLXlsxUtil.baseFileName(extendedData) + ".xlsx")
    file.delete()
    workbook.write(new FileOutputStream(file))

    file.getName
  }
}
