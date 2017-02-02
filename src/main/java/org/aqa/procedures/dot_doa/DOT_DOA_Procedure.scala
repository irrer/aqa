package org.aqa.procedures.dot_doa

import java.io.File
import org.aqa.Util
import org.apache.poi.ss.usermodel.Cell
import org.aqa.run.ProcedureStatus
import scala.xml.Elem
import scala.xml.Node
import scala.xml.PrettyPrinter
import org.aqa.web.WebUtil
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import org.aqa.db.ProcedureOutput

object DOT_DOA_Procedure {

    // val curDir = new File(".") // TODO reinstate
    private val curDir = new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490""") // TODO rm

    private def terminate(status: ProcedureStatus.Value, msg: String): Unit = {
        println(msg)
        ProcedureStatus.writeProcedureStatus(status)
        System.exit(0)
    }

    private def getExcelFile: Workbook = {
        val parentDir = curDir.getParentFile

        println("Using parent directory: " + parentDir.getAbsolutePath)
        val excel = parentDir.listFiles.map(af => ExcelUtil.read(af)).filter { ws => ws.isRight }.map(o => o.right.get)
        if (excel.isEmpty) terminate(ProcedureStatus.fail, "No Excel files found")
        if (excel.size > 1) terminate(ProcedureStatus.fail, "Expecting one Excel file but found " + excel.size)
        println("Found excel file");
        excel.head
    }

    private def isSectionHeader(cell: Cell): Boolean = {
        (cell != null) && ExcelUtil.cellToString(cell).startsWith("Section")
    }

    /** Get a sorted list of cell columns.  Returns (row, list)*/
    private def getColumnList(sheet: Sheet): (Int, Seq[Int]) = {
        def tryHeader(rownum: Int) = ExcelUtil.cellList(sheet.getRow(rownum)).filter(c => isSectionHeader(c)).map(c => c.getAddress.getColumn).sorted
        val headers = (1 to 10).filter(r => sheet.getRow(r) != null).map(r => (r, tryHeader(r))).sortWith((a, b) => a._2.size > b._2.size).head
        if (headers._2.isEmpty) terminate(ProcedureStatus.fail, "No Section headers found")
        headers
    }

    private def locToXml(valueLabel: String, row: Row, col: Int, sheet: Sheet): Elem = {
        val leaf = row.getCell(0).getNumericCellValue.toInt.toString
        val loc = row.getCell(col).getNumericCellValue.toString
        (<Value Leaf={ leaf }>{ loc }</Value>).copy(label = valueLabel)
    }

    private def colToXml(lbl: String, col: Int, colId: Int, sheet: Sheet, rowList: Seq[Int]): Elem = {
        <Section id={ colId.toString }>
            { rowList.map(row => locToXml(lbl, sheet.getRow(row), col, sheet)) }
        </Section>
    }

    /** Get a sorted list of cell rows indexes. */
    private def getRowList(sheet: Sheet, headerRow: Int): Seq[Int] = {
        def isLeafRow(row: Row): Boolean = {
            (row != null) &&
                (row.getCell(0) != null) &&
                (row.getCell(0).getCellTypeEnum == CellType.NUMERIC) &&
                (row.getCell(0).getNumericCellValue == row.getCell(0).getNumericCellValue.toInt)
        }
        (headerRow + 1 until 1000).toList.takeWhile { rownum => isLeafRow(sheet.getRow(rownum)) }.sorted
    }

    private def get(sheet: Sheet, mainLabel: String, valueLabel: String): Elem = {
        println("Processing sheet " + sheet.getSheetName)
        val headers = getColumnList(sheet)
        val rowList = getRowList(sheet, headers._1)
        (<Main>
             { headers._2.zipWithIndex.map(colInd => colToXml(valueLabel, colInd._1, colInd._2 + 1, sheet, rowList)) }
         </Main>).copy(label = mainLabel)
    }

    private def xmlToText(document: Node): String = new PrettyPrinter(1024, 2).format(document)

    private def getSheet(workbook: Workbook, nameList: Iterable[String]): Option[Sheet] = {
        nameList.map(name => workbook.getSheet(name)).filter(s => s != null).headOption
    }

    private class SheetSpec(val nameList: Seq[String], val mainLabel: String, val valueLabel: String, val required: Boolean) {

        def process(workbook: Workbook): Option[Elem] = {
            getSheet(workbook, nameList) match {
                case Some(sheet) => Some(get(sheet, mainLabel, valueLabel))
                case None if required => { terminate(ProcedureStatus.fail, "Required sheet " + nameList + " not found"); None }
                case _ => None
            }
        }
    }

    /**
     * Extract DOT-DOA values from Excel spreadsheet and put into XML form for consumption by database.  Also convert
     * spreadsheet to HTML for viewing by user.
     */
    def main(args: Array[String]): Unit = {
        val workbook = getExcelFile

        val sheetSpecList: List[SheetSpec] = List(
            new SheetSpec(Seq("LOC", "Sheet1"), "LeafOffsetCorrectionList", "LeafOffsetCorrection_mm", true),
            new SheetSpec(Seq("Trans", "Transmission", "Sheet1"), "LeafTransmissionList", "LeafTransmission_pct", true)
            )

        val elemList = sheetSpecList.map(ss => ss.process(workbook)).flatten

        val xml = {
            <DOT_DOA outputPK={ System.getenv("outputPK") }>
                { elemList }
            </DOT_DOA>
        }

        val text = xmlToText(xml)
        Util.writeFile(new File(curDir, ProcedureOutput.outputFileName), text)

        val html = WebUtil.excelToHtml(workbook)

        val outFile = new File(curDir, "display.html")
        Util.writeFile(outFile, html)
        println("wrote Excel as HTML")
    }

}
