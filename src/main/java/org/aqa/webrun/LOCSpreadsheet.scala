package org.aqa.webrun

import scala.xml.XML
import java.io.File
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import org.aqa.procedures.ProcedureOutputUtil
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.xssf.streaming.SXSSFWorkbook
import org.apache.poi.xssf.streaming.SXSSFRow
import org.apache.poi.ss.usermodel.CellType

/**
 * Extract LOC related values from XML file.
 */
class LOCSpreadsheet(dir: File, locXml: LOCXml) {

}

object LOCSpreadsheet {
    def main(args: Array[String]): Unit = {
        val dir = new File("""D:\AQA_Data\results\results\TBD_2\CHIC1_11\Leaf_Offset_and_Transmission_1.0.0_2\2017-05-11T18-19-43-765_111\output_2017-05-11T18-19-43-801""")
        val locXml = new LOCXml(dir)

        val workbook = new SXSSFWorkbook

        val leafOffsetConstancySheet = workbook.createSheet("Leaf Offset Constancy")

        val row: SXSSFRow = null
        val cell = row.createCell(1, CellType.NUMERIC)
        cell.setCellValue(12.345)

        //  workbook.createSheet("Leaf Transmission")
        //  workbook.createSheet("R Squared")

    }
}
