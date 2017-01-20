//package org.aqa.webrun

import org.restlet.Request
import org.restlet.Response
import slick.driver.PostgresDriver.api._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging._
import org.aqa.db.Machine
import edu.umro.ScalaUtil.Trace._
import java.io.File
import org.aqa.db.Procedure
import org.aqa.run.Run
import org.aqa.Util
import org.aqa.web.WebUtil
import org.restlet.Restlet
import org.apache.poi._
import java.io.FileInputStream

import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFRow
import org.apache.poi.xssf.usermodel.XSSFCell
import org.apache.poi.ss.usermodel.CellType

object ReadExcelXSSF {

    def getRowList(sheet: XSSFSheet): Seq[XSSFRow] = {
        def getRow(rowIndex: Int): Option[XSSFRow] = {
            try {
                Some(sheet.getRow(rowIndex))
            }
            catch {
                case t: Throwable => None
            }
        }
        val rowList = (sheet.getFirstRowNum to sheet.getLastRowNum).map(r => getRow(r)).flatten
        rowList
    }

    def cellToString(cell: XSSFCell): String = {

        if (cell == null) "null"
        else {
            cell.getCellTypeEnum match {
                case CellType._NONE =>
                    "none"
                case CellType.NUMERIC =>
                    cell.getNumericCellValue.toString
                case CellType.STRING =>
                    cell.getStringCellValue
                case CellType.FORMULA =>
                    cell.getStringCellValue
                case CellType.BLANK =>
                    "blank"
                case CellType.BOOLEAN =>
                    cell.getBooleanCellValue.toString
                case CellType.ERROR =>
                    "error"
                case _ =>
                    "Unknown cell type"
            }
        }
    }

    /** (sheet, row, column), cell */
    type CellMapT = Map[(Int, Int, Int), XSSFCell]
    val emptyCellMap: CellMapT = Map[(Int, Int, Int), XSSFCell]()

    def flattenSeqMap[A, B](seq: Seq[Map[A, B]]): Map[A, B] = seq.foldLeft(Map[A, B]())((a, c) => a ++ c)

    def getCellList(sheetIndex: Int, row: XSSFRow): CellMapT = {
        def getCell(cellIndex: Int): Option[XSSFCell] = {
            try {
                row.getCell(cellIndex) match {
                    case cell: XSSFCell => Some(cell)
                    case _ => None
                }
            }
            catch {
                case t: Throwable => None
            }
        }
        if (row == null) emptyCellMap
        else {
            val cellList = (row.getFirstCellNum to row.getLastCellNum).map(c => getCell(c)).flatten
            val cellMap = cellList.map(c => ((sheetIndex, c.getRowIndex, c.getColumnIndex), c)).toMap
            cellMap
        }
    }

    def getCellList(sheetIndex: Int, sheet: XSSFSheet): CellMapT = {
        val seq = getRowList(sheet).map(row => getCellList(sheetIndex, row))
        flattenSeqMap(seq)
    }

    def getCellList(workbook: XSSFWorkbook): CellMapT = {
        val seq = (0 until workbook.getNumberOfSheets).map(s => getCellList(s, workbook.getSheetAt(s)))
        flattenSeqMap(seq)
    }

    def main(args: Array[String]): Unit = {

        val fileName = """D:\tmp\aqa\extract\Summary U of M of Data 2016-06-14.xlsx"""

        val fis = new FileInputStream(new File(fileName))

        val workbook = new XSSFWorkbook(fis)

        val cellMap = getCellList(workbook)
        cellMap.map(c => println(c._1._1 + " : " + c._1._2 + " : " + c._1._3 + " : " + c._2))
    }

}

