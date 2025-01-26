package org.aqa.webrun.wl.isoCheck

import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.WLIsoCheckHTML

import java.io.File
import java.io.FileOutputStream
import java.util.Date

object WLXLSXSpreadsheet extends Logging {

  /** First row of Data sheet content (inclusive). */
  private val firstRowNum = 3

  /** Last row of Data sheet content (inclusive). */
  private val lastRowNum = 17

  /**
    * Put the data into a copy of the XLSX template file and write a new *.xlxs file.
    *
    * The new file is put into the isoCheck subdirectory.
    *
    * This file can serve as a double check as to the correctness of the calculations.
    *
    * @param extendedData Metadata data.
    * @param pairList WL data and DICOM.
    * @param isoTable Processed isoTable data.
    * @param collimator Processed column data.
    * @return The name of the file.
    */
  def makeSpreadsheet(extendedData: ExtendedData, pairList: Seq[WLBeam], isoTable: WLIsoTable, collimator: WLCollimator): String = {

    val workbook = new XSSFWorkbook(Config.WLIsoCheckTemplateFile)

    // get the date+time of the first slice
    val firstDateTime: Date = pairList.head.acquisition

    val columnList = WLColumnList(extendedData.machine, firstDateTime).columnList

    // the Data worksheet
    val sheetData: XSSFSheet = workbook.getSheetAt(1)

    def updateTitleRow(): Unit = {
      val row = sheetData.getRow(0)

      val dataDateCell = row.getCell(1)
      val dataDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)
      dataDateCell.setCellValue(s"Data Date: $dataDateText")

      val analysisDateCell = row.getCell(2)
      val analysisDateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.analysisDate.get)
      analysisDateCell.setCellValue(s"Data Date: $analysisDateText")
    }

    def updateContentRow(rowNum: Int, pair: WLBeam): Unit = {
      val row = sheetData.getRow(rowNum - 1) // subtracting 1 converts a row number to a 0-relative row index

      columnList.indices.foreach(i => columnList(i).updateCell(row.getCell(i), pair.wl, pair.al))
    }

    /**
      * Update all the content rows.  Sort by acquisition time, associate each data set with a row, and then process each row.
      */
    def updateContentRowList(): Unit = {
      (firstRowNum to lastRowNum).foreach(rowNum => updateContentRow(rowNum, pairList(rowNum - firstRowNum)))
    }

    /**
      * Update the values in the Analysis sheet that are calculated using gradient descent
      */
    def updateAnalysisSheet(): Unit = {
      val sheetAnalysis = workbook.getSheetAt(3)

      val row14 = sheetAnalysis.getRow(13)
      row14.getCell(11).setCellValue(isoTable.get_dXT__0_Optimized)
      row14.getCell(12).setCellValue(isoTable.get_dZT__0_Optimized)
      row14.getCell(13).setCellValue(isoTable.get_IsoTable_X_Optimized)
      row14.getCell(14).setCellValue(isoTable.get_IsoTable_Z_Optimized)
    }

    /**
      * Update the values in the Collimator sheet that are calculated using gradient descent
      */
    def updateCollimatorSheet(): Unit = {
      val sheetAnalysis = workbook.getSheetAt(4)

      val row4 = sheetAnalysis.getRow(3) // zero relative addressing 3 -> 4
      row4.getCell(7).setCellValue(collimator.getColl_X_Optimized)
      row4.getCell(8).setCellValue(collimator.getColl_Z_Optimized)
    }

    def update(): Unit = {
      // turn on auto sizing for all columns
      updateTitleRow()
      updateContentRowList()
      updateAnalysisSheet()
      updateCollimatorSheet()
    }

    update()

    val file = new File(WLIsoCheckHTML.dir(extendedData), WLXlsxUtil.baseFileName(extendedData) + ".xlsx")
    file.delete()
    workbook.write(new FileOutputStream(file))
    logger.info("Wrote WL IsoCheck spreadsheet file " + file.getAbsolutePath)

    file.getName
  }
}
