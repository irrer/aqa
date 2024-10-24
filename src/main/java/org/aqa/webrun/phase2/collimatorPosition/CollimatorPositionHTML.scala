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

package org.aqa.webrun.phase2.collimatorPosition

import org.aqa.Util
import org.aqa.db.CollimatorPosition
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebServer
import org.aqa.Config
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq

import java.awt.image.BufferedImage
import java.io.File
import scala.xml.Elem

object CollimatorPositionHTML {
  val htmlFileName = "CollimatorPosition.html"

  val subDirName = "CollimatorPosition"

  /**
    * Generate a detailed report and write it to the output directory.  Also write a CSV file.  Return an
    * HTML snippet that serves as a summary and a link to the detailed report.
    */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[(CollimatorPosition, BufferedImage)], crashList: Seq[String], status: ProcedureStatus.Value): Elem = {

    val resultDataList = resultList.map(cpb => cpb._1)
    CollimatorPositionCSV.makeCsvFile(extendedData, runReq, resultDataList)

    val csvFileReference = {
      <a title={"Download " + CollimatorPositionAnalysis.subProcedureName + " as CSV File"} href={CollimatorPositionCSV.csvFileName}>CSV</a>
    }

    val viewRtPlan = {
      val planHref = Phase2Util.dicomViewHref(runReq.rtplan, extendedData, runReq)
      <a title="View RT Plan DICOM file" href={planHref}>RT Plan</a>
    }

    class Col(val title: String, name: String, val get: (CollimatorPosition, BufferedImage) => Any) {
      def toHeader = <th title={title}>{name}</th>
      def toRow(psnChk: CollimatorPosition, bufImg: BufferedImage) = <td title={title}>{get(psnChk, bufImg).toString}</td>
    }

    def degree(deg: Double): String = (Util.modulo360(deg).round.toInt % 360).formatted("%4d")

    def leaf(diff: Double): String = diff.formatted("%8.3f")

    class ColSide(override val title: String, name: String, override val get: (CollimatorPosition, BufferedImage) => Double) extends Col(title, name, get) {
      override def toRow(collimatorPosition: CollimatorPosition, bufImg: BufferedImage) = {

        val value = get(collimatorPosition, bufImg)
        val pass = Config.CollimatorCenteringTolerence_mm >= value.abs
        val text = value.formatted("%8.3f")

        val elem = {
          if (pass)
            <td title={title}>{text}</td>
          else
            <td title={title} class="danger">{text}</td>
        }
        elem
      }
    }

    class ColBeamName(override val title: String, name: String, override val get: (CollimatorPosition, BufferedImage) => String) extends Col(title, name, get) {
      override def toRow(collimatorPosition: CollimatorPosition, bufImg: BufferedImage) = {
        val pngFileName = {
          val floodComp = if (collimatorPosition.FloodCompensation) "_with_flood_comp" else ""
          collimatorPosition.beamName.replace(" ", "_") + floodComp + ".png"
        }
        val subDir = new File(extendedData.output.dir, subDirName)
        Util.mkdirs(subDir)
        val pngFile = new File(subDir, pngFileName)
        Util.writePng(bufImg, pngFile)
        val pngUrl = WebServer.urlOfResultsFile(pngFile)
        val pass = collimatorPosition.status.toString.equals(ProcedureStatus.pass.toString)
        val text = " " + collimatorPosition.beamName + " : " + (if (pass) "Pass" else "Fail")
        val link = { <a href={pngUrl}>{text}</a> }
        val elem = {
          if (pass) { <td title={title}>{link}</td> }
          else { <td class="danger" title={title}>{link}</td> }
        }
        elem
      }
    }

    val rowList = Seq(
      new ColBeamName("View image with annotated " + WebUtil.titleNewline + " edge measurements", "Beam", (psnChk: CollimatorPosition, bufImg: BufferedImage) => psnChk.beamName),
      new Col("Flood field compensation used: True/False", "Flood Comp.", (psnChk: CollimatorPosition, bufImg: BufferedImage) => (if (psnChk.FloodCompensation) "T" else "F")),
      new Col("Gantry Angle in degrees", "Gantry", (psnChk: CollimatorPosition, bufImg: BufferedImage) => degree(psnChk.gantryAngle_deg)),
      new Col("Collimator Angle in degrees", "Collimator", (psnChk: CollimatorPosition, bufImg: BufferedImage) => degree(psnChk.collimatorAngle_deg)),
      new ColSide("Expected X1 - X1 edge in image, in mm", "X1 mm", (psnChk: CollimatorPosition, bufImg: BufferedImage) => psnChk.X1_ExpectedMinusImage_mm),
      new ColSide("Expected X2 - X2 edge in image, in mm", "X2 mm", (psnChk: CollimatorPosition, bufImg: BufferedImage) => psnChk.X2_ExpectedMinusImage_mm),
      new ColSide("Expected Y1 - Y1 edge in image, in mm", "Y1 mm", (psnChk: CollimatorPosition, bufImg: BufferedImage) => psnChk.Y1_ExpectedMinusImage_mm),
      new ColSide("Expected Y2 - Y2 edge in image, in mm", "Y2 mm", (psnChk: CollimatorPosition, bufImg: BufferedImage) => psnChk.Y2_ExpectedMinusImage_mm)
    )

    def collimatorPositionTableHeader: Elem = {
      <thead><tr>{rowList.map(row => row.toHeader)}</tr></thead>
    }

    def collimatorPositionToTableRow(collimatorPosition: CollimatorPosition, bufImg: BufferedImage): Elem = {
      <tr>{rowList.map(row => row.toRow(collimatorPosition, bufImg))}</tr>
    }

    val tbody = resultList.map(psnChk => collimatorPositionToTableRow(psnChk._1, psnChk._2))

    val crashTable = {
      def stringToRow(s: String) = { <tr><td>{s}</td></tr> }
      <div>
        <h3>Crashed Beams</h3>
        <table class="table table-striped">
          <tbody>{crashList.map(c => stringToRow(c))}</tbody>
        </table>
      </div>
    }

    val collimatorCenterOfRotation = {
      def fmt(d: Double) = d.formatted("%7.3f")
      val text = fmt(resultList.head._1.XCollimatorCenterOfRotation_mm) + ", " + fmt(resultList.head._1.YCollimatorCenterOfRotation_mm)
      <span title="X,Y coordinates of collimator center of rotation">Collimator center of rotation {text}</span>
    }

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row" style="margin:20px;">
          <div class="col-md-1">{csvFileReference}</div>
          <div class="col-md-1">{viewRtPlan}</div>
          <div class="col-md-2">   {collimatorCenterOfRotation}</div>
        </div>
        <div class="row" style="margin:20px;">
          <table class="table table-striped">
            {collimatorPositionTableHeader}
            <tbody>{tbody}</tbody>
          </table>
          {if (crashList.nonEmpty) crashTable}
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, CollimatorPositionAnalysis.subProcedureName, status, None, runReq.rtimageMap)
    val file = new File(extendedData.output.dir, htmlFileName)
    Util.writeBinaryFile(file, text.getBytes)

    /**
      * Make a tiny summary and link to the detailed report.
      */
    def makeSummary = {
      val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
      val elem = {
        <div title="Click for details.">
          <a href={htmlFileName}>
            Collimator Position<br/>
            <img src={iconImage} height="32"/>
          </a>
        </div>
      }
      elem
    }

    makeSummary
  }

}
