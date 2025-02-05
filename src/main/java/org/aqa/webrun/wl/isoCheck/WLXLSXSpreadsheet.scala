package org.aqa.webrun.wl.isoCheck

import edu.umro.ScalaUtil.Trace
import org.apache.poi.ss.util.CellReference
import org.apache.poi.xssf.usermodel.XSSFCell
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

  private def nullifyCell(sheet: XSSFSheet, rowIndex: Int, cellIndex: String): Unit = {
    val intIndex = CellReference.convertColStringToIndex(cellIndex)

    val row = sheet.getRow(rowIndex - 1)
    if (row != null) {
      val cell: XSSFCell = row.getCell(intIndex)
      if (cell != null) {
        try {
          cell.setBlank()
        } catch {
          case t: Throwable =>
            Trace.trace(s"=====badness ${sheet.getSheetName}      rowIndex: $rowIndex     cellIndex: $cellIndex : " + fmtEx(t))
        }
      }
    }
  }

  private def nullifyRow(sheet: XSSFSheet, rowIndex: Int, firstCell: String, lastCell: String): Unit = {
    val lo = CellReference.convertColStringToIndex(firstCell)
    val hi = CellReference.convertColStringToIndex(lastCell)

    (lo to hi).foreach(c => nullifyCell(sheet, rowIndex, CellReference.convertNumToColString(c)))
  }

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
  def makeSpreadsheet(extendedData: ExtendedData, pairList: Seq[WLBeam], isoTable: Option[WLIsoTable], collimator: WLCollimator): String = {

    val workbook = new XSSFWorkbook(Config.WLIsoCheckTemplateFile)

    // get the date+time of the first slice
    val firstDateTime: Date = pairList.head.dataDate

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
      val last = Math.min(lastRowNum, pairList.size + firstRowNum - 1)
      (firstRowNum to last).foreach(rowNum => updateContentRow(rowNum, pairList(rowNum - firstRowNum)))
    }

    def removeUnusedDataRows(): Unit = {
      if (isoTable.isEmpty) {
        (12 to 17).foreach(rowIndex => nullifyRow(sheetData, rowIndex, "A", "Y"))
      }
    }

    def removeUnusedPreprocessRows(): Unit = {
      if (isoTable.isEmpty) {
        val sheetPreprocess = workbook.getSheetAt(2)
        (12 to 17).foreach(rowIndex => nullifyRow(sheetPreprocess, rowIndex, "A", "V"))
      }
    }

    def updateSNCImportSheet(): Unit = {
      if (isoTable.isEmpty) {
        val sheetSNCImport = workbook.getSheetAt(0)
        nullifyCell(sheetSNCImport, 5, "C")
        nullifyCell(sheetSNCImport, 6, "C")
        nullifyCell(sheetSNCImport, 10, "C")
      }
    }

    /**
      * Update the values in the Analysis sheet that are calculated using gradient descent
      */
    def updateAnalysisSheet(): Unit = {
      val sheetAnalysis = workbook.getSheetAt(3)
      if (isoTable.isDefined) {
        val row14 = sheetAnalysis.getRow(13)
        row14.getCell(11).setCellValue(isoTable.get.get_dXT__0_Optimized)
        row14.getCell(12).setCellValue(isoTable.get.get_dZT__0_Optimized)
        row14.getCell(13).setCellValue(isoTable.get.get_IsoTable_X_Optimized)
        row14.getCell(14).setCellValue(isoTable.get.get_IsoTable_Z_Optimized)
      } else {
        // remove table references
        nullifyRow(sheetAnalysis, 5, "U", "X")
        nullifyRow(sheetAnalysis, 12, "K", "S")
        (12 to 21).foreach(rowIndex => nullifyRow(sheetAnalysis, rowIndex, "A", "R"))
      }
    }

    /**
      * Update the values in the Collimator sheet that are calculated using gradient descent
      */
    def updateCollimatorSheet(): Unit = {
      val sheetCollimator = workbook.getSheetAt(4)

      val row4 = sheetCollimator.getRow(3) // zero relative addressing 3 -> 4
      row4.getCell(7).setCellValue(collimator.getColl_X_Optimized)
      row4.getCell(8).setCellValue(collimator.getColl_Z_Optimized)
    }

    def updateReportSheet(): Unit = {
      if (isoTable.isEmpty) {
        val sheetReport = workbook.getSheetAt(5)

        nullifyCell(sheetReport, 4, "F")
        nullifyCell(sheetReport, 4, "G")
        nullifyCell(sheetReport, 4, "K")
      }
    }

    def update(): Unit = {
      // turn on auto sizing for all columns
      updateTitleRow()

      updateContentRowList()
      removeUnusedDataRows()
      removeUnusedPreprocessRows()
      updateSNCImportSheet()
      updateAnalysisSheet()
      updateCollimatorSheet()
      updateReportSheet()
    }

    update()

    val file = new File(WLIsoCheckHTML.dir(extendedData), WLXlsxUtil.baseFileName(extendedData) + ".xlsx")
    file.delete()
    workbook.write(new FileOutputStream(file))
    logger.info("Wrote WL IsoCheck spreadsheet file " + file.getAbsolutePath)

    file.getName
  }
}
