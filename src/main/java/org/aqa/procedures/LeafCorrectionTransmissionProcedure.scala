/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.procedures

import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.Workbook
import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebUtil
import org.aqa.webrun.LOC.LOCUtil

import java.io.File
import scala.xml.Elem
import scala.xml.Node
import scala.xml.PrettyPrinter

object LeafCorrectionTransmissionProcedure extends Logging {

  def curDir = new File(System.getProperty("user.dir"))

  private def getExcelFile: Workbook = {
    println("Using curDir: " + curDir.getAbsolutePath)
    val parentDir = curDir.getParentFile
    println("Using parent directory: " + parentDir.getAbsolutePath)
    println("Looking for the Excel file in: " + curDir.list.toSeq.mkString(" "))
    val excel = curDir.listFiles.map(af => (af, ExcelUtil.read(af))).filter { fws => fws._2.isRight }.map(fo => (fo._1, fo._2.right.get))
    if (excel.isEmpty) ProcedureStatus.terminate("No Excel files found", ProcedureStatus.fail)
    val fileNameList = excel.foldLeft("")((t, fws) => t + "  " + fws._1.getAbsolutePath)
    if (excel.size > 1) ProcedureStatus.terminate("Expecting one Excel file but found " + excel.size + " : " + fileNameList, ProcedureStatus.fail)
    println("Using excel file " + fileNameList);
    excel.head._2
  }

  private def isSectionHeader(cell: Cell): Boolean = {
    (cell != null) && ExcelUtil.cellToString(cell).startsWith("Section")
  }

  /** Get a sorted list of cell columns.  Returns (row, list)*/
  private def getColumnList(sheet: Sheet): (Int, Seq[Int]) = {
    def tryHeader(rownum: Int) = ExcelUtil.cellList(sheet.getRow(rownum)).filter(c => isSectionHeader(c)).map(c => c.getAddress.getColumn).sorted
    val headers = (1 to 10).filter(r => sheet.getRow(r) != null).map(r => (r, tryHeader(r))).sortWith((a, b) => a._2.size > b._2.size).head
    if (headers._2.isEmpty) ProcedureStatus.terminate("No Section headers found", ProcedureStatus.fail)
    headers
  }

  private def locToXml(valueLabel: String, row: Row, col: Int, sheet: Sheet): Elem = {
    val leaf = row.getCell(0).getNumericCellValue.toInt.toString
    val loc = row.getCell(col).getNumericCellValue.toString
    (<Value Leaf={leaf}>{loc}</Value>).copy(label = valueLabel)
  }

  private def colToXml(lbl: String, col: Int, colId: Int, sheet: Sheet, rowList: Seq[Int]): Elem = {
    <Section id={colId.toString}>
      {rowList.map(row => locToXml(lbl, sheet.getRow(row), col, sheet))}
    </Section>
  }

  /** Get a sorted list of cell rows indexes. */
  private def getRowList(sheet: Sheet, headerRow: Int): Seq[Int] = {
    def isLeafRow(row: Row): Boolean = {
      (row != null) &&
      (row.getCell(0) != null) &&
      //(row.getCell(0).getCellTypeEnum == CellType.NUMERIC) &&
      (row.getCell(0).getCellType == CellType.NUMERIC) &&
      (row.getCell(0).getNumericCellValue == row.getCell(0).getNumericCellValue.toInt)
    }
    (headerRow + 1 until 1000).toList.takeWhile { rownum => isLeafRow(sheet.getRow(rownum)) }.sorted
  }

  private def get(sheet: Sheet, mainLabel: String, valueLabel: String): Elem = {
    println("Processing sheet " + sheet.getSheetName)
    val headers = getColumnList(sheet)
    val rowList = getRowList(sheet, headers._1)
    (<Main>
       {headers._2.zipWithIndex.map(colInd => colToXml(valueLabel, colInd._1, colInd._2 + 1, sheet, rowList))}
     </Main>).copy(label = mainLabel)
  }

  private def xmlToText(document: Node): String = new PrettyPrinter(1024, 2).format(document)

  private def getSheet(workbook: Workbook, nameList: Iterable[String]): Option[Sheet] = {
    nameList.map(name => workbook.getSheet(name)).filter(s => s != null).headOption
  }

  private class SheetSpec(val nameList: Seq[String], val mainLabel: String, val valueLabel: String, val required: Boolean) {

    def process(workbook: Workbook): Option[Elem] = {
      getSheet(workbook, nameList) match {
        case Some(sheet)      => Some(get(sheet, mainLabel, valueLabel))
        case None if required => { ProcedureStatus.terminate("Required sheet " + nameList + " not found", ProcedureStatus.fail); None }
        case _                => None
      }
    }
  }

  private def excelToXml(workbook: Workbook) = {
    val sheetSpecList: List[SheetSpec] = List(
      new SheetSpec(Seq("LOC", "Sheet1"), "LeafOffsetCorrectionList", "LeafOffsetCorrection_mm", true),
      new SheetSpec(Seq("Trans", "Transmission", "Sheet2"), "LeafTransmissionList", "LeafTransmission_pct", true)
    )

    val elemList = sheetSpecList.map(ss => ss.process(workbook)).flatten

    val xml = {
      <Output outputPK={System.getenv("outputPK")}>
        {elemList}
      </Output>
    }

    val text = xmlToText(xml)
    Util.writeFile(new File(curDir, LOCUtil.locXmlOutputFileName), text)
  }

  /**
    * Extract DOT-DOA values from Excel spreadsheet and put into XML form for consumption by database.  Also convert
    * spreadsheet to HTML for viewing by user.
    */
  def main(args: Array[String]): Unit = {
    try {
      val workbook = getExcelFile

      // excelToXml(workbook)

      val html = WebUtil.excelToHtml(workbook)

      val outFile = new File(curDir, "spreadsheet.html")
      Util.writeFile(outFile, html)
      println("wrote Excel as HTML")
    } catch {
      case t: Throwable => ProcedureStatus.terminate("Unexpected error: " + fmtEx(t), ProcedureStatus.abort)
    }
  }

}
