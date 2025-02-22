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

package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import edu.umro.ScalaUtil.Trace
import org.apache.poi.ss.util.CellReference
import org.aqa.db.IsoCheck
import org.aqa.db.Output
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.Util
import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.MetadataCache
import org.aqa.webrun.phase2.phase2csv.Phase2Csv
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA
import org.aqa.Logging

import java.io.File
import scala.xml.Node
import scala.xml.XML

class IsoCheckCsv(metadataCache: MetadataCache) extends Phase2Csv[IsoCheck.IsoCheckHistory](metadataCache: MetadataCache) {

  override val dataName: String = "IsoCheck"

  private def CA_X(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"CA-X G$g C$c T$t"
    val description = s"CA-X Gantry:$g Collimator:$c Table$t (mm) =Analysis!F$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c, t) match {
          case Some(wl) if wl.caX.isDefined => wl.caX.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_Y(g: Int, c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-Y G$g C$c T0"
    val description = s"CA-Y Gantry:$g Collimator:$c Table:0 (mm) =Analysis!G$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c) match {
          case Some(wl) if wl.caY.isDefined => wl.caY.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_Z(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"CA-Z G$g C$c T$t"
    val description = s"CA-Z Gantry:$g Collimator:$c Table$t (mm) =Analysis!H$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c, t) match {
          case Some(wl) if wl.caZ.isDefined => wl.caZ.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_ZT(t: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-Z G180 C270 T$t"
    val description = s"CA-Z Gantry:180 Collimator:270 Table$t (mm) =Analysis!G$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if wl.caZ.isDefined => wl.caZ.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_Xpp(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-X'' G180 C$c T0"
    val description = s"CA-X'' G180 C$c T0 (mm) =Collimator!K$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Xpp(WLBeam(wl), ic.collimator.get_Coll_X_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def CA_Zpp(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-Z'' G180 C$c T0"
    val description = s"CA-X'' G180 C$c T0 (mm) =Collimator!K$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Zpp(WLBeam(wl), ic.collimator.get_Coll_Z_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def CA_RppSq(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-R''^2 G180 C$c T0"
    val description = s"CA-R''^2 G180 C$c T0 (mm) =Collimator!L$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Rpp(WLBeam(wl), ic.collimator.get_Coll_X_Optimized, ic.collimator.get_Coll_Z_Optimized)
          case _ => NA
        }
      }
    )
  }

  override protected def makeColList: Seq[CsvCol[IC]] = {
    SNCImport.sncImportList() ++
      AnalysisSummary.analysisSummaryList() ++
      BBXZpp.analysisBBpp() ++
      AnalysisTableDxDz.tableDxDz() ++
      AnalysisBBXZ.tableBBXZ() ++
      AnalysisMLC.mlc() ++
      AnalysisMLCdXdY.mlcDxDy() ++
      CollimatorRXZpp.optimizedAndBBXZ() ++
      XYOffset.XYOffsetList() ++
      TableAxisXZ.tableAxisXZ()
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[IC] = {
    val cdHistory = IsoCheck.history(machinePK)
    cdHistory
  }

  override def getSopUidList(data: IC): Seq[String] = {
    val firstBeam = data.getBeam(0, 90).get.rtimageUID
    Seq(firstBeam)
  }

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

  override def getOutput(data: IC): Output = data.output
}

object IsoCheckCsv extends Logging {

  /** Abbreviation for the long name */
  type IC = IsoCheck.IsoCheckHistory

  /** Not applicable */
  val NA = "NA"

  private val sheetNameList: Seq[String] = Seq(
    "SNCImport",
    "Data",
    "Preprocess",
    "Analysis",
    "Collimator",
    "Report",
    "Instructions"
  )

  private case class CSVSheet(name: String) {
    private val file = new File("""D:\tmp\wl\csv\""" + name + ".csv")

    private val text = Util.readTextFile(file).right.get

    private val rowList = text.replaceAll("\r", "").split("\n")

    def getValue(rowIndex: Int, colIndex: Int): Double = {
      val cellList = Util.csvToText(rowList(rowIndex))
      val cellText = cellList(colIndex)
      cellText.toDouble
    }
  }

  private val sheetList: Seq[CSVSheet] = sheetNameList.map(CSVSheet)

  private val maxDiff = 0.0000000001

  // private val xlsxFile: File = new File("""D:\tmp\wl\csv\IsoCheck_TB1_2025-02-03_19-06.xlsx""")
  // private val workbook: XSSFWorkbook = new XSSFWorkbook(xlsxFile)
  // println(s"Read workbook from  $xlsxFile")

  private val downloadFile: File = new File("""D:\tmp\wl\csv\19_06_00.csv""")

  private val downloadValueTextList: Seq[String] = {
    val text = Util.readTextFile(downloadFile).right.get
    text.split(",")
  }

  private def valuesMatch(a: Double, b: Double): Boolean = {
    (a == b) || {
      val dif = ((a - b) / a).abs
      val ok = dif < maxDiff
      ok
    }
  }

  private case class CellRef(text: String, parent: ColDef) {
    private val colLetter: String = text.split("!")(1).split("[0-9]").head
    private val rowNumber: Int = {
      val list = text.split("[A-Z]")
      val rowText = list.last.replaceAll("[^0-9]", "")
      rowText.toInt
    }
    private val sheetName: String = text.tail.split("!").head

    private val colIndex: Int = CellReference.convertColStringToIndex(colLetter)
    private val rowIndex: Int = rowNumber - 1

    def xlsxValue: Double = {
      val sheet = sheetList.find(_.name.equals(sheetName)).get
      sheet.getValue(rowIndex, colIndex)
    }

    override def toString: String = {
      s"=$sheetName!$colLetter$rowNumber"
    }

  }

  private case class ColDef(tr: Node) {
    private val list = tr \ "td"

    private val address: String = list.head.text
    private val name: String = list(1).text
    private val definition: String = list(2).text

    private val columnIndex = CellReference.convertColStringToIndex(address)

    val refList: Seq[CellRef] = {
      val list = definition.split(" ").filter(_.matches("=.*!.*")).toSeq
      list.map(cellText => CellRef(cellText, this))
    }

    val value = downloadValueTextList(columnIndex)

    override def toString: String = {
      s"""$address   ||  $name  ||  ${refList.mkString(" | ")}"""
    }

    println(s"Defined col: $this     tr: $tr")
    Trace.trace()
  }

  private val dropHead = 6
  private val dropTail = 33

  private def readColDef(): Seq[ColDef] = {
    val file = new File("""D:\tmp\wl\csv\ColumnDef.html""")
    val content = XML.loadFile(file)
    val list = {
      val nodeList = content \ "body" \ "div" \ "table" \ "tr"

      nodeList.drop(dropHead).dropRight(dropTail).map(n => ColDef(n))
    }
    list
  }

  private def showCellRef(cellRef: CellRef): String = {
    println(s"Showing cell $cellRef")
    try {
      val parentValue = cellRef.parent.value.toDouble
      val cellRefValue = cellRef.xlsxValue
      val ok = valuesMatch(parentValue, cellRefValue)
      if (!ok)
        Trace.trace(s"$cellRef  : ${cellRef.parent}    Badness bad match AQA: $parentValue !=  XLSX: $cellRefValue")

      s"$ok $CellRef"
    } catch {
      case t: Throwable =>
        Trace.trace(s"$cellRef  : ${cellRef.parent}    Badness exception: ${fmtEx(t)}")
        "Fail"
    }
  }

  private def showColDef(colDef: ColDef): Unit = {
    println(s"Showing col $colDef")
    val cellRefList = colDef.refList.map(showCellRef)
    println(s"$colDef\n    ${cellRefList.mkString("\n    ")}")
  }

  def main(args: Array[String]): Unit = {

    val colDefList = readColDef()

    if (false) {
      val sheet = sheetList.find(_.name.equals("Analysis")).get
      val v = sheet.getValue(3, 7)
      Trace.trace(v)
    }

    colDefList.foreach(showColDef)
  }
}
