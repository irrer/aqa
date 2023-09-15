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

package org.aqa.webrun.LOC

import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.util.CellAddress
import org.apache.poi.xssf.streaming.SXSSFCell
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.xssf.streaming.SXSSFSheet
import org.apache.poi.xssf.streaming.SXSSFWorkbook
import org.aqa.Util
import org.aqa.db.Input
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MultileafCollimator
import org.aqa.db.Output
import org.aqa.web.ViewOutput
import org.aqa.webrun.LOCSpreadsheet
import org.aqa.webrun.LOCXml
import org.restlet.Response

import java.io.File
import java.io.FileOutputStream
import scala.collection.immutable

/**
  * Extract LOC related values from XML file.
  */
case class LOCMakeSpreadsheet(outputDir: File, locXml: LOCToXml, response: Response) {

  private def newRowFromSheet(sheet: SXSSFSheet): SXSSFRow = {
    val rowNum = if (sheet.rowIterator.hasNext) sheet.getActiveCell.getRow + 1 else 0
    sheet.setActiveCell(new CellAddress(rowNum, 0))
    val row = sheet.createRow(rowNum)
    row
  }

  def cellNum(row: SXSSFRow): Int = if (row.getLastCellNum < 0) 0 else row.getLastCellNum

  def textCell(row: SXSSFRow, content: String): SXSSFCell = {
    val cell = row.createCell(cellNum(row), CellType.STRING)
    cell.setCellValue(content)
    cell
  }

  def numCell(row: SXSSFRow, content: Double): SXSSFCell = {
    val cell = row.createCell(cellNum(row), CellType.NUMERIC)
    cell.setCellValue(content)
    cell
  }

  def leafAndSectionHeaders(row: SXSSFRow): immutable.IndexedSeq[SXSSFCell] = {
    textCell(row, "Leaf Number")
    (1 to 5).map(s => textCell(row, "Section " + s))
  }

  def rowMainNumbers(sheet: SXSSFSheet, values: Seq[Seq[Double]], extra: (SXSSFRow, Int) => Unit): immutable.Seq[Unit] = {
    def addRow(l: Int): Unit = {
      val row = newRowFromSheet(sheet)
      val i = l - locXml.leafIndexList.head
      numCell(row, l)
      (0 until locXml.sections).map(s => numCell(row, values(i)(s)))
      extra(row, i)
    }
    locXml.leafIndexList.map(l => addRow(l))
  }

  def addRowNums(row: SXSSFRow, name: String, values: Seq[Double]): Seq[SXSSFCell] = {
    textCell(row, name)
    values.map(v => numCell(row, v))
  }

  /**
    * Make first sheet.
    */
  private def sheetLeafOffsetConstancy(workbook: SXSSFWorkbook) = {
    val sheet = workbook.createSheet("Leaf Offset Constancy")
    def newRow = newRowFromSheet(sheet)

    val output = Output.get(locXml.outputPK).get
    val input = Input.get(output.inputPK).get
    val machine = Machine.get(output.machinePK.get).get
    val institution = Institution.get(machine.institutionPK).get
    val multileafCollimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val dicomList = outputDir.getParentFile.listFiles.toSeq.filter { f => f.getName.toLowerCase.endsWith(".dcm") }.map(df => Util.readDicomFile(df)).filter(m => m.isRight).map(al => al.right.get)

    def dicomString(tag: AttributeTag): Option[String] = {
      val s = dicomList.head.get(tag).getSingleStringValueOrNull
      Option(s)
    }

    def dicomStringDflt(tag: AttributeTag, dflt: String): String = { dicomString(tag) match { case Some(s) => s; case _ => dflt } }

    def row1 = {
      val row = newRow
      textCell(row, "Calculated Leaf Offset Constancy in 5 Sections Each MLC Leaf")
      (0 until 5).map(_ => textCell(row, ""))
      textCell(row, "Report: ")
      val url = response.getRequest.getHostRef.toString + ViewOutput.path + "?outputPK=" + output.outputPK.get
      textCell(row, url)
    }

    def row2 = {
      val row = newRow

      textCell(row, "Institution: ")
      textCell(row, institution.name)

      textCell(row, "MLC Type: ")
      textCell(row, multileafCollimator.model)

      textCell(row, "Date of Test: ")
      val dataDate = input.dataDate match { case Some(date) => Util.timeHumanFriendly(date); case _ => "unknown" }
      textCell(row, dataDate)
    }

    def row3 = {
      val row = newRow
      val epidSize: String = {
        try {
          dicomString(TagFromName.Rows).get + "x" + dicomString(TagFromName.Columns).get
        } catch {
          case _: Throwable => "unknown"
        }
      }

      textCell(row, "Machine: ")
      textCell(row, machine.id)

      textCell(row, "EPID Size: ")
      textCell(row, epidSize)

      textCell(row, "Date of Analysis: ")
      textCell(row, Util.currentTimeHumanFriendly)
    }

    def row4 = {
      val row = newRow

      textCell(row, "Station: ")
      textCell(row, dicomStringDflt(TagFromName.StationName, "unknown"))

      textCell(row, "Serial No.: " + dicomStringDflt(TagFromName.DeviceSerialNumber, "unknown"))
      textCell(row, "LOC Leaf Averages")
      textCell(row, "EPID Center Correction in mm: ")
      numCell(row, locXml.epidCenterCorrection_mm)
    }

    def row5 = {
      val row = newRow
      leafAndSectionHeaders(row)
      textCell(row, "Mean Across Leaf")
      textCell(row, "Range Across Leaf")

    }

    //        def rowMainNumbersX = {
    //            def addRow(l: Int) = {
    //                val row = newRow
    //                val i = l - locXml.leafIndexList.head
    //                numCell(row, l)
    //                (0 until locXml.sections).map(s => numCell(row, locXml.LeafOffsetConstancyValue(i)(s)))
    //                numCell(row, locXml.LeafOffsetConstancyMean(i))
    //                numCell(row, locXml.LeafOffsetConstancyRange(i))
    //            }
    //            locXml.leafIndexList.map(l => addRow(l))
    //        }

    def rowMainNums = {
      def extra(row: SXSSFRow, i: Int): Unit = {
        numCell(row, locXml.LeafOffsetConstancyMean(i))
        numCell(row, locXml.LeafOffsetConstancyRange(i))
      }
      rowMainNumbers(sheet, locXml.LeafOffsetConstancyValue, extra)
    }

    def rowSummaryNumbers = {
      addRowNums(newRow, "Mean", locXml.LeafOffsetConstancySectionMean)
      addRowNums(newRow, "STD", locXml.LeafOffsetConstancySectionSTD)
      addRowNums(newRow, "Coeff. of Var.", locXml.LeafOffsetConstancySectionCoeffOfVar)
      addRowNums(newRow, "Range", locXml.LeafOffsetConstancySectionRange)
    }

    // build the (mutable) worksheet one piece at a time.
    row1
    row2
    row3
    row4
    row5
    rowMainNums
    newRow
    rowSummaryNumbers
  }

  private def sheetTransmission(workbook: SXSSFWorkbook) = {
    val sheet = workbook.createSheet("Transmission")
    def newRow = newRowFromSheet(sheet)

    def rowTitle = {
      textCell(newRow, "Transmission Data")
      newRow
      newRow
      val row = newRow
      (1 to 3).map(_ => textCell(row, ""))
      textCell(row, "Trans Leaf Averages")
    }

    def rowHeader = {
      val row = newRow
      leafAndSectionHeaders(row)
      textCell(row, "Mean Across Sections")
    }

    def rowMainNums = {
      def extra(row: SXSSFRow, i: Int): Unit = {
        if (i < locXml.LeafOffsetTransmissionMean.size)
          numCell(row, locXml.LeafOffsetTransmissionMean(i))
      }
      rowMainNumbers(sheet, locXml.LeafOffsetTransmissionValue, extra)
    }

    def rowSummaryNumbers = {
      addRowNums(newRow, "Mean", locXml.LeafOffsetTransmissionSectionMean)
      addRowNums(newRow, "STD", locXml.LeafOffsetTransmissionSectionSTD)
      addRowNums(newRow, "Coeff. of Var.", locXml.LeafOffsetTransmissionSectionCoeffOfVar)
      addRowNums(newRow, "Range", locXml.LeafOffsetTransmissionSectionRange)
    }

    rowTitle
    rowHeader
    rowMainNums
    newRow
    rowSummaryNumbers
  }

  private def basicSheet(workbook: SXSSFWorkbook, sheetName: String, title: String, values: Seq[Seq[Double]]) = {
    val sheet = workbook.createSheet(sheetName)
    def newRow = newRowFromSheet(sheet)

    def rowTitle = {
      textCell(newRow, title)
      newRow
      newRow
    }

    def rowHeader = leafAndSectionHeaders(newRow)

    def rowMainNums = {
      //noinspection ScalaUnusedSymbol
      def extra(row: SXSSFRow, i: Int): Unit = {}
      rowMainNumbers(sheet, values, extra)
    }

    rowTitle
    rowHeader
    rowMainNums
  }

  private def sheetRSquared(workbook: SXSSFWorkbook) = basicSheet(workbook, "R Squared", "Calculated R Squared Value", locXml.LOCRSquared)

  private def sheetDiffOpen(workbook: SXSSFWorkbook) = basicSheet(workbook, "Diff Open", "Calculated Difference From Baseline OPEN image", locXml.LOCDifferenceFromBaselineOpen)

  private def sheetDiffTrans(workbook: SXSSFWorkbook) = basicSheet(workbook, "Diff Trans", "Calculated Difference From Baseline TRANS image", locXml.LOCDifferenceFromBaselineTrans)

  private def makeWorkbook: SXSSFWorkbook = {
    val workbook = new SXSSFWorkbook
    sheetLeafOffsetConstancy(workbook)
    sheetTransmission(workbook)
    sheetRSquared(workbook)
    sheetDiffOpen(workbook)
    sheetDiffTrans(workbook)
    workbook

  }

  /** Write the spreadsheet to the given location.  If it already exists, the old copy will first be deleted. */
  def write(outFile: File): Unit = {
    // Make the workbook before deleting the old file in case something goes wrong.  That way you will still have the old workbook.
    val workbook = makeWorkbook
    outFile.delete
    val fos = new FileOutputStream(outFile)
    workbook.write(fos)
    fos.flush()
  }

  /** Write the spreadsheet to the standard location.  If it already exists, the old copy will first be deleted. */
  def write(): Unit = {
    write(new File(outputDir, LOCMakeSpreadsheet.spreadsheetFileName))
  }

}

object LOCMakeSpreadsheet {

  val spreadsheetFileName = "spreadsheet.xlsx"

  def main(args: Array[String]): Unit = {
    println("starting")
    val dir = new File("""D:\AQA_Data\results\TBD_2\CHIC1_11\Leaf_Offset_and_Transmission_1.0.0_2\2017-05-16T18-01-19-140_126\output_2017-05-16T18-01-19-195""")
    val locXml = new LOCXml(dir)
    val start = System.currentTimeMillis
    new LOCSpreadsheet(dir, locXml, null).write
    println("done.  Elapsed ms: " + (System.currentTimeMillis - start))

    //  workbook.createSheet("Leaf Transmission")
    //  workbook.createSheet("R Squared")

  }
}
