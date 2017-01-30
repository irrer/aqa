package org.aqa.procedures

import edu.umro.MSOfficeUtil.Excel.ReadExcel._
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
import edu.umro.MSOfficeUtil.Excel.ReadExcel

object LeafOffsetCorrectionProcedure {

    private val statusFileName = "status.txt"

    private def terminate(status: ProcedureStatus.Value, msg: String): Unit = {
        println(msg)
        Util.writeFile(new File(statusFileName), status.toString)
        System.exit(0)
    }

    private def getExcelFile: Workbook = {
        //val parentDir = new File(".").getParentFile
        //val parentDir = new File("""D:\AQA_Data\docs\xls\leaf_offset2""")
        val parentDir = new File("""D:\AQA_Data\docs\xls\foo""")

        println("Using parent directory: " + parentDir.getAbsolutePath)
        val excel = parentDir.listFiles.map(af => read(af)).filter { ws => ws.isRight }.map(o => o.right.get)
        if (excel.isEmpty) terminate(ProcedureStatus.fail, "No Excel files found")
        if (excel.size > 1) terminate(ProcedureStatus.fail, "Expecting one Excel file but found " + excel.size)
        println("Found excel file");
        excel.head
    }

    private def isInt(ws: CellMapT, s: Int, r: Int, c: Int): Boolean = {
        try {
            val cell = ws(new CellCoord(s, r, c))
            cell.getNumericCellValue.toInt
            true
        }
        catch {
            case t: Throwable => false
        }
    }

    private def isSectionHeader(cell: Cell): Boolean = {
        (cell != null) && cellToString(cell).startsWith("Section")
    }

    /** Get a sorted list of cell columns. */
    private def getColumnList(sheet: Sheet): Seq[Int] = {
        val headers = ReadExcel.cellList(sheet.getRow(4)).filter(c => isSectionHeader(c)).map(c => c.getAddress.getColumn).sorted
        if (headers.isEmpty) terminate(ProcedureStatus.fail, "No Section headers found")
        headers
    }

    private def locToXml(row: Row, col: Int, sheet: Sheet): Elem = {
        val leaf = row.getCell(0).getNumericCellValue.toInt.toString
        val loc = row.getCell(col).getNumericCellValue.toString
        <LeafOffsetCorrection Leaf={ leaf }>{ loc }</LeafOffsetCorrection>
    }

    private def colToXml(col: Int, colId: Int, sheet: Sheet, rowList: Seq[Int]): Elem = {
        <Section id={ colId.toString }>
            { rowList.map(row => locToXml(sheet.getRow(row), col, sheet)) }
        </Section>
    }

    /** Get a sorted list of cell rows indexes. */
    private def getRowList(sheet: Sheet): Seq[Int] = {
        def isLeafRow(row: Row): Boolean = {
            (row != null) &&
                (row.getCell(0) != null) &&
                (row.getCell(0).getCellTypeEnum == CellType.NUMERIC)
        }
        (5 until 1000).toList.takeWhile { rownum => isLeafRow(sheet.getRow(rownum)) }.sorted
    }

    private def getLeafOffsetCorrection(sheet: Sheet): Elem = {
        val rowList = getRowList(sheet)
        val loc = { //
            <LeafOffsetCorrectionList outputPK={ System.getenv("outputPK") }>
                { getColumnList(sheet).zipWithIndex.map(colInd => colToXml(colInd._1, colInd._2 + 1, sheet, rowList)) }
            </LeafOffsetCorrectionList>
        }
        loc
    }

    def xmlToText(document: Node): String = new PrettyPrinter(1024, 2).format(document)

    def main(args: Array[String]): Unit = {
        val ws = getExcelFile
        val sheet = ws.getSheet("LOC")

        val text = xmlToText(getLeafOffsetCorrection(sheet))
        println(text)
        Util.writeFile(new File("results_LeafOffsetCorrection.xml"), text)

        val html = WebUtil.excelToHtml(ws)

        val outFile = new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490\output.html""")
        Util.writeFile(outFile, html)
        println("wrote Excel as HTML")
    }

    private def undup = {
        def hashFile(file: File): List[Byte] = Util.secureHash(Util.readBinaryFile(file).right.get).toList

        case class FS(file: File, chksum: List[Byte]);

        val dirName = """D:\AQA_Data\docs\xls"""

        val candList = new File(dirName).listFiles.filter(f => f.isFile).map(rf => new FS(rf, hashFile(rf)))

        val dupList = candList.groupBy { x => x.chksum }.filter(g => g._2.size > 1).map(g1 => g1._2.map(fs => fs.file))

        val sList = dupList.map(dl => dl.sortWith((a, b) => a.getName.size < b.getName.size)).map(f => f.tail)

        val delList = sList.flatten.map(f => println("rm '" + f.getName + "'"))

        println("Number of deletes: " + delList.size)
    }

}