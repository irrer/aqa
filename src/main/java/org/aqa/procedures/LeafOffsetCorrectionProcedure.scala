package org.aqa.procedures

import edu.umro.MSOfficeUtil.Excel.ReadExcel._
import java.io.File
import org.aqa.Util
import org.apache.poi.ss.usermodel.Cell
import org.aqa.run.ProcedureStatus
import scala.xml.Elem
import scala.xml.Node
import scala.xml.PrettyPrinter

object LeafOffsetCorrectionProcedure {

    private val statusFileName = "status.txt"

    private def terminate(status: ProcedureStatus.Value, msg: String): Unit = {
        println(msg)
        Util.writeFile(new File(statusFileName), status.toString)
        System.exit(0)
    }

    private def getExcelFile: CellMapT = {
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

    /** Get a sorted list of cell rows. */
    private def getRowList(ws: CellMapT): Seq[Int] = {
        val firstLeafNum = ws(new CellCoord(0, 5, 0)).getNumericCellValue.toInt
        (5 until 1000).toList.takeWhile { x => isInt(ws, 0, x, 0) }.sorted
    }

    private def isSectionHeader(coord: CellCoord, cell: Cell): Boolean = {
        (coord.sheet == 0) &&
            (coord.col > 0) &&
            cellToString(cell).startsWith("Section")
    }

    /** Get a sorted list of cell columns. */
    private def getColumnList(ws: CellMapT): Seq[Int] = {
        val headers = ws.filter(c => isSectionHeader(c._1, c._2)).map(c1 => c1._1.col).toList.sorted
        if (headers.isEmpty) terminate(ProcedureStatus.fail, "No Section headers found")
        headers
    }

    private def locToXml(row: Int, col: Int, ws: CellMapT): Elem = {
        val leaf = ws(new CellCoord(0, row, 0)).getNumericCellValue.toInt
        val dataCell = ws(new CellCoord(0, row, col))
        <LeafOffsetCorrection Leaf={ leaf.toString }>{ dataCell.getNumericCellValue.toString.trim }</LeafOffsetCorrection>
    }

    private def colToXml(col: Int, colId: Int, ws: CellMapT, rowList: Seq[Int]): Elem = {
        <Section id={ colId.toString }>
            { rowList.map(row => locToXml(row, col, ws)) }
        </Section>
    }

    private def getLeafOffsetCorrection(ws: CellMapT): Elem = {
        val rl = getRowList(ws)
        val loc = { //
            <LeafOffsetCorrectionList outputPK={ System.getenv("outputPK") }>
                { getColumnList(ws).zipWithIndex.map(colInd => colToXml(colInd._1, colInd._2 + 1, ws, rl)) }
            </LeafOffsetCorrectionList>
        }
        loc
    }

    def xmlToText(document: Node): String = new PrettyPrinter(1024, 2).format(document)

    def main(args: Array[String]): Unit = {
        val ws = getExcelFile
        val text = xmlToText(getLeafOffsetCorrection(ws))
        println(text)
        Util.writeFile(new File("results_LeafOffsetCorrection.xml"), text)

        val maxCol = ws.filter(f => f._1.sheet == 0).map(c => c._1.col).max
        val maxRow = ws.filter(f => f._1.sheet == 0).map(c => c._1.row).max

        def doCell(row: Int, col: Int): Elem = {
            val content: String = try {
                cellToString(ws(new CellCoord(0, row, col)))
            }
            catch {
                case t: Throwable => ""
            }
            <td>{ content }</td>
        }

        def doRow(row: Int): Elem = {
            <tr>{ (0 to maxCol).map(c => doCell(row, c)) }</tr>
        }

        val lead = "<!DOCTYPE html>\n"

        val html = {
            <html>
                <head>
                </head>
                <body>
                    <table>
                        { (0 to maxRow).map(c => doRow(c)) }
                    </table>
                </body>
            </html>
        }

        val htmlText = lead + xmlToText(html)
        Util.writeFile(new File("output.html"), htmlText)
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