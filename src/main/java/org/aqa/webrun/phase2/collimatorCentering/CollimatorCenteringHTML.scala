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

package org.aqa.webrun.phase2.collimatorCentering

import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.run.ProcedureStatus
import org.aqa.web.C3ChartHistory
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.File
import scala.xml.Elem

object CollimatorCenteringHTML {

  private val htmlFileName = "CollimatorCentering.html"

  private def fmt(d: Double): String = d.formatted("%8.3f")

  private def showImage(fileName: String, outputDir: File, bufImg: BufferedImage, id: String): Elem = {
    val fn = FileUtil.replaceInvalidFileNameCharacters(fileName, '_')
    val pngFile = new File(outputDir, fn)
    Util.writePng(bufImg, pngFile)
    <div>
      <center id={id}>
        <a href={fn}>
          <img src={fn} class="img-responsive"/>
        </a>
      </center>
    </div>
  }

  private def makeTable(collimatorCentering: CollimatorCentering): Elem = {
    val cc = collimatorCentering
    case class CCRow(title: String, value: Double)

    val rowList = Seq(
      CCRow("X collimator center", cc.xCollimatorCenter_mm),
      CCRow("Y collimator center", cc.yCollimatorCenter_mm),
      CCRow("X1 90 degrees", cc.X1_090_mm),
      CCRow("X2 90 degrees", cc.X2_090_mm),
      CCRow("Y1 90 degrees", cc.Y1_090_mm),
      CCRow("Y2 90 degrees", cc.Y2_090_mm),
      CCRow("X1 270 degrees", cc.X1_270_mm),
      CCRow("X2 270 degrees", cc.X2_270_mm),
      CCRow("Y1 270 degrees", cc.Y1_270_mm),
      CCRow("Y2 270 degrees", cc.Y2_270_mm)
    )

    def rowToHtml(ccRow: CCRow): Elem = {
      <tr>
        <td>{ccRow.title}</td>
        <td>{fmt(ccRow.value)}</td>
      </tr>
    }

    val table = {
      <table class="table table-striped" title="All values in mm">
        <tr>
          <th>Description</th>
          <th>Value</th>
        </tr>
        {rowList.map(row => rowToHtml(row))}
      </table>
    }
    table
  }

  /**
    * Make a tiny summary and link to the detailed report.
    */
  private def makeSummary(status: ProcedureStatus.Value, resultSummary: String) = {
    val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={htmlFileName}>
          {CollimatorCenteringAnalysis.subProcedureName}<br/>
          {resultSummary}<br/>
          <img src={iconImage} height="32"/>
        </a>
      </div>
    }
    elem
  }

  def makeDisplay(
      extendedData: ExtendedData,
      collimatorCentering: CollimatorCentering,
      status: ProcedureStatus.Value,
      image090: MeasureTBLREdges.AnalysisResult,
      image270: MeasureTBLREdges.AnalysisResult,
      runReq: RunReq
  ): Elem = {

    val charts = new CollimatorCenteringChart(extendedData.output.outputPK.get)

    val attr090 = runReq.rtimageMap(Config.CollimatorCentering090BeamName)
    val attr270 = runReq.rtimageMap(Config.CollimatorCentering270BeamName)

    val translator090 = new IsoImagePlaneTranslator(attr090)
    val translator270 = new IsoImagePlaneTranslator(attr270)

    val isoCenter090 = translator090.pix2Iso(image090.measurementSet.center)
    val isoCenter270 = translator270.pix2Iso(image270.measurementSet.center)

    val outputDir = extendedData.output.dir

    val resultSummary = fmt(collimatorCentering.xCollimatorCenter_mm) + ", " + fmt(collimatorCentering.yCollimatorCenter_mm)

    def imageTitle(collAngle: Int, isoCenter: Point2D.Double): Elem = {
      val isoCenterText = fmt(isoCenter.getX) + ", " + fmt(isoCenter.getY)
      val title = "Collimator Angle " + collAngle + " : image center in isoplane " + isoCenterText
      <h3 title={title} style="text-align:center;">{collAngle.toString + " : " + isoCenterText}</h3>
    }

    val image090Name = "image090"
    val image270Name = "image270"

    val script = {
      s"""
    <script>
      $$(document).ready(function(){ $$('#$image090Name').zoom(); });
      $$(document).ready(function(){ $$('#$image270Name').zoom(); });
    </script>
    ${CollimatorCenteringChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)}
"""
    }

    val content = {
      val href090 = Phase2Util.dicomViewHref(attr090, extendedData, runReq)
      val href270 = Phase2Util.dicomViewHref(attr270, extendedData, runReq)

      <div>
        <div class="row">
          <div class="col-md-4 col-md-offset-3" align="middle">
            <h3 title='X, Y difference from isoplane center in mm'>Offset: {resultSummary} mm</h3>
          </div>
          <div class="col-md-10 col-md-offset-1">
            <div class="row">
              {C3ChartHistory.htmlHelp()}
            </div>
            <div class="row" style=" border: 1px solid grey; margin-bottom:10px;">
              <center><h2>Summary</h2></center>
              {charts.summary.html}
            </div>
            <div class="row" style=" border: 1px solid grey; margin-bottom:10px;">
              <center><h2>{Config.CollimatorCentering090BeamName}</h2></center>
              {charts.collCenter090.html}
            </div>
            <div class="row" style=" border: 1px solid grey; margin-bottom:10px;">
              <center><h2>{Config.CollimatorCentering270BeamName}</h2></center>
              {charts.collCenter270.html}
            </div>
          </div>
        </div>
        <div class="row" style="margin:30px;">
          <div class="col-md-4 col-md-offset-1" align="middle">
            {imageTitle(90, isoCenter090)}
            <a title='Click for DICOM details' href={href090}>{Config.CollimatorCentering090BeamName}<br/></a>
            {showImage("CollimatorCentering090_" + Config.CollimatorCentering090BeamName + ".png", outputDir, image090.bufferedImage, image090Name)}
          </div>
          <div class="col-md-4" align="middle">
            {imageTitle(270, isoCenter270)}
            <a title='Click for DICOM details' href={href270}>{Config.CollimatorCentering270BeamName}<br/></a>
            {showImage("CollimatorCentering270_" + Config.CollimatorCentering270BeamName + ".png", outputDir, image270.bufferedImage, image270Name)}
          </div>
        </div>
        <div class="col-md-4 col-md-offset-4" align="middle">
          {makeTable(collimatorCentering)}
        </div>
      </div>
    }

    val html = Phase2Util.wrapSubProcedure(
      extendedData,
      content,
      "Collimator Centering",
      status,
      Some(script),
      runReq
    )
    val outFile = new File(outputDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status, resultSummary)
  }

}
