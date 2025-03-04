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

import org.apache.poi.ss.util.CellReference
import org.aqa.db.IsoCheck
import org.aqa.db.Output
import org.aqa.Util
import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.MetadataCache
import org.aqa.webrun.phase2.phase2csv.Phase2Csv
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA
import org.aqa.Logging

import java.io.File
import scala.annotation.tailrec
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
        ic.wlMap.find(g, c, t) match {
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
        ic.wlMap.find(g, c) match {
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
        ic.wlMap.find(g, c, t) match {
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
        ic.wlMap.find(180, 270, t) match {
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
        ic.wlMap.find(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Xpp(wl, ic.collimator.get_Coll_X_Optimized)
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
        ic.wlMap.find(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Zpp(wl, ic.collimator.get_Coll_Z_Optimized)
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
        ic.wlMap.find(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Rpp(wl, ic.collimator.get_Coll_X_Optimized, ic.collimator.get_Coll_Z_Optimized)
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
    val firstBeam = data.wlMap.find(0, 90).get.rtimageUID
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

  private def csvLineToList(line: String): Seq[String] = {
    @tailrec
    def next(text: String, list: Seq[String] = Seq()): Seq[String] = {
      val commaIndex = text.indexOf(",")
      if (commaIndex == -1)
        list
      else {
        val valueText: String = text.take(commaIndex)
        next(text.drop(commaIndex + 1), list :+ valueText)
      }
    }
    next(line + ",")
  }

  private case class CSVSheet(name: String) {
    private val file = new File("""D:\tmp\wl\csv\""" + name + ".csv")

    private val text = Util.readTextFile(file).right.get

    private val rowList = text.replaceAll("\r", "").split("\n")

    def getValue(rowIndex: Int, colIndex: Int): String = {
      val cellList = csvLineToList(rowList(rowIndex))
      val cellText = cellList(colIndex)
      cellText
    }
  }

  private val sheetList: Seq[CSVSheet] = sheetNameList.map(CSVSheet)

  private val maxDiff = 0.0000000001

  // private val xlsxFile: File = new File("""D:\tmp\wl\csv\IsoCheck_TB1_2025-02-03_19-06.xlsx""")
  // private val workbook: XSSFWorkbook = new XSSFWorkbook(xlsxFile)
  // println(s"Read workbook from  $xlsxFile")

  private val valueList: Seq[String] = {
    val valueFile: File = new File("""D:\tmp\wl\csv\19_06_00.csv""")

    val fullText = Util.readTextFile(valueFile).right.get.trim

    csvLineToList(fullText)
  }

  /*
  private val downloadValueTextList: Seq[String] = {
    val text = Util.readTextFile(downloadFile).right.get
    text.split(",")
  }
   */

  private def doublesMatch(a: Double, b: Double): Boolean = {
    (a == b) || {
      val dif = ((a - b) / a).abs
      val ok = dif < maxDiff
      ok
    }
  }

  private case class CellRef(text: String, parent: Definition) {
    private val colLetter: String = text.split("!")(1).split("[0-9]").head
    private val rowNumber: Int = {
      val list = text.split("[A-Z]")
      val rowText = list.last.replaceAll("[^0-9]", "")
      rowText.toInt
    }
    val sheetName: String = text.tail.split("!").head

    val colIndex: Int = CellReference.convertColStringToIndex(colLetter)
    val rowIndex: Int = rowNumber - 1

    override def toString: String = {
      s"=$sheetName!$colLetter$rowNumber"
    }

  }

  /**
    * Definition for a column.  Extracted from Column Definitions on "Index of CSV Files" page.
    * @param tr HTML node.
    */
  private case class Definition(tr: Node, index: Int) {
    private val list = tr \ "td"

    private val address: String = list.head.text
    private val name: String = list(1).text
    private val definition: String = list(2).text

    val refTextList = definition.split(" ").filter(_.matches("=.*!.*")).toSeq

    val refList: Seq[CellRef] = {
      refTextList.map(cellText => CellRef(cellText, this))
    }

    // val value = downloadValueTextList(columnIndex)

    override def toString: String = {
      s"""${(" " + address).takeRight(2)}   ||  $name  ||  ${refTextList.mkString(" | ")}"""
    }

    println(s"Defined col: $this")
  }

  private def readDefinition(): Seq[Definition] = {
    val file = new File("""D:\AQA_Data\results\CSV\IsoCheck.html""")
    val text = Util.readTextFile(file).right.get.replace("<!DOCTYPE html>", "").trim
    val content = XML.loadString(text)
    val list = {
      val nodeList = content \ "body" \ "div" \ "table" \ "tr"

      // do tail to drop the header
      nodeList.tail.zipWithIndex.map(nodeIndex => Definition(nodeIndex._1, nodeIndex._2))
    }
    list
  }

  private def verifyCellRef(cellRef: CellRef): Boolean = {

    val csvIndex = cellRef.parent.index
    val csvText = valueList(csvIndex).trim

    val csvLetterColumn = (" " + CellReference.convertNumToColString(csvIndex)).takeRight(2)

    val excelText = {
      val sheet = sheetList.find(_.name.equals(cellRef.sheetName)).get
      val text = sheet.getValue(cellRef.rowIndex, cellRef.colIndex)
      text.trim
    }

    val same = csvText.equals(excelText) ||
      doublesMatch(csvText.toDouble, excelText.toDouble)

    val sameText = if (same) "ok  " else "fail"

//    if (!same)
//      Trace.trace()
    println(s"""$sameText : csvIndex: ${csvIndex.formatted("%3d")} = $csvLetterColumn   csv value: $csvText    excel: $excelText    $cellRef ::  ${cellRef.parent}""")

    same
  }

  def main(args: Array[String]): Unit = {

    println("=====================================================")

    val definitionList = readDefinition()

    println("=====================================================")

    valueList.indices.foreach(i => println(s"$i : ${CellReference.convertNumToColString(i)} : ${valueList(i)}"))

    val refList = definitionList.flatMap(_.refList)

    val numberOfMatches: Int = refList.map(verifyCellRef).count(v => v)

    println(s"Size of refList: ${refList.size}     Number of matches: $numberOfMatches")

  }
}
