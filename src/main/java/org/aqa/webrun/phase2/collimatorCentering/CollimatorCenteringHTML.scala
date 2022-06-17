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
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.File
import scala.xml.Elem

object CollimatorCenteringHTML {

  private val htmlFileName = "CollimatorCentering.html"

  private def fmt(d: Double): String = d.formatted("%8.3f")

  private def idOf(a: Int) = "Gantry" + a.formatted("%03d")
  private def id090(ga: Int) = idOf(ga) + "C090"
  private def id270(ga: Int) = idOf(ga) + "C270"

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
  private def makeSummary(status: ProcedureStatus.Value, analysisResultList: Seq[CollimatorCenteringAnalysis.AnalysisResult]) = {
    val worst = analysisResultList.maxBy(r => r.collimatorCentering.center.distance(new Point2D.Double(0, 0))).collimatorCentering.center
    val summary = <span>Collimator Centering<br/>{Util.fmtDbl(worst.getX) + ", " + Util.fmtDbl(worst.getY)}</span>
    val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={htmlFileName}>
          {CollimatorCenteringAnalysis.subProcedureName}<br/>
          {summary}<br/>
          <img src={iconImage} height="32"/>
        </a>
      </div>
    }
    elem
  }

  /**
    * Make the three history charts for this gantry angle.
    * @param outputPK Output being referenced.
    * @param gantryAngle Gantry angle rounded to nearest 90 degrees.
    * @return
    */
  private def makeCharts(outputPK: Long, gantryAngle: Int): Seq[Elem] = {

    val charts = new CollimatorCenteringChart(outputPK, gantryAngle = gantryAngle)

    val help = {
      <div class="row">
        {C3ChartHistory.htmlHelp()}
      </div>
    }

    val summary = {
      <div class="row" style=" border: 1px solid grey; margin-bottom:10px;">
        <center>
          <h2>Summary</h2>
        </center>{charts.summary.html}
      </div>
    }

    val beam090 = {
      <div class="row" style=" border: 1px solid grey; margin-bottom:10px;">
        <center>
          <h2>
            {Config.CollimatorCentering090BeamName}
          </h2>
        </center>{charts.collCenter090.html}
      </div>
    }

    val beam270 = {
      <div class="row" style=" border: 1px solid grey; margin-bottom:10px;">
        <center>
          <h2>
            {Config.CollimatorCentering270BeamName}
          </h2>
        </center>{charts.collCenter270.html}
      </div>
    }

    Seq(help, summary, beam090, beam270)
  }

  private def makeTabContent(
      extendedData: ExtendedData,
      analysisResult: CollimatorCenteringAnalysis.AnalysisResult,
      runReq: RunReq
  ): Elem = {

    val gantryAngle = analysisResult.collimatorCentering.gantryAngleRounded_deg

    // shortcuts for more concise references
    val outputPK = extendedData.output.outputPK.get
    val image090 = analysisResult.measureTBLREdges090
    val image270 = analysisResult.measureTBLREdges270
    val collimatorCentering = analysisResult.collimatorCentering

    val attr090 = runReq.rtimageMap(Config.CollimatorCentering090BeamName)
    val attr270 = runReq.rtimageMap(Config.CollimatorCentering270BeamName)

    val translator090 = new IsoImagePlaneTranslator(attr090)
    val translator270 = new IsoImagePlaneTranslator(attr270)

    val isoCenter090 = translator090.pix2Iso(image090.measurementSet.center)
    val isoCenter270 = translator270.pix2Iso(image270.measurementSet.center)

    val outputDir = extendedData.output.dir

    val resultSummary = fmt(analysisResult.collimatorCentering.xCollimatorCenter_mm) + ", " + fmt(analysisResult.collimatorCentering.yCollimatorCenter_mm)

    def imageTitle(collAngle: Int, isoCenter: Point2D.Double): Elem = {
      val isoCenterText = fmt(isoCenter.getX) + ", " + fmt(isoCenter.getY)
      val title = "Collimator Angle " + collAngle + " : image center in isoplane " + isoCenterText
      <h3 title={title} style="text-align:center;">{collAngle.toString + " : " + isoCenterText}</h3>
    }

    val content = {
      val href090 = Phase2Util.dicomViewHref(attr090, extendedData, runReq)
      val href270 = Phase2Util.dicomViewHref(attr270, extendedData, runReq)

      <div id={idOf(gantryAngle)} class="tab-pane fade in active">
        <div class="row">
          <div class="col-md-4 col-md-offset-3" align="middle">
            <h3 title='X, Y difference from isoplane center in mm'>Offset: {resultSummary} mm</h3>
          </div>
          <div class="col-md-10 col-md-offset-1">
            {makeCharts(outputPK, gantryAngle = gantryAngle)}
          </div>
        </div>
        <div class="row" style="margin:30px;">
          <div class="col-md-4 col-md-offset-1" align="middle">
            {imageTitle(90, isoCenter090)}
            <a title='Click for DICOM details' href={href090}>{Config.CollimatorCentering090BeamName}<br/></a>
            {showImage("CollimatorCentering090_" + Config.CollimatorCentering090BeamName + ".png", outputDir, image090.bufferedImage, id090(gantryAngle))}
          </div>
          <div class="col-md-4" align="middle">
            {imageTitle(270, isoCenter270)}
            <a title='Click for DICOM details' href={href270}>{Config.CollimatorCentering270BeamName}<br/></a>
            {showImage("CollimatorCentering270_" + Config.CollimatorCentering270BeamName + ".png", outputDir, image270.bufferedImage, id270(gantryAngle))}
          </div>
        </div>
        <div class="col-md-4 col-md-offset-4" align="middle">
          {makeTable(collimatorCentering)}
        </div>
      </div>
    }

    content
  }

  private def makeTab(index: Int, result: CollimatorCenteringAnalysis.AnalysisResult): Elem = {
    val angle = result.collimatorCentering.gantryAngleRounded_deg
    <li class={if (index == 0) "active" else ""}>
      <a data-toggle="tab" href={"#" + idOf(angle)}>Gantry {angle}</a>
    </li>
  }

  private def makeScript(gantryAngleList: Seq[Int], outputPK: Long): String = {

    def pair(ga: Int) = {
      s"""
      $$(document).ready(function(){ $$('#${id090(ga)}').zoom(); });
      $$(document).ready(function(){ $$('#${id270(ga)}').zoom(); });
"""
    }

    val script = {
      s"""
    <script>
      ${gantryAngleList.map(pair).mkString("\n  ")}
      ${gantryAngleList.map(ga => CollimatorCenteringChartHistoryRestlet.makeReference(outputPK, ga)).mkString("\n  ")}
    </script>
"""
    }

    script
  }

  def makeDisplay(
      extendedData: ExtendedData,
      status: ProcedureStatus.Value,
      analysisResultList: Seq[CollimatorCenteringAnalysis.AnalysisResult],
      runReq: RunReq
  ): Elem = {

    val content = {
      <div>
        <div>
          <ul class="nav nav-tabs">
            {analysisResultList.zipWithIndex.map(a => makeTab(a._2, a._1))}
          </ul>
        </div>
        <div class="tab-content">
          {analysisResultList.map(a => makeTabContent(extendedData, a, runReq))}
        </div>
      </div>
    }

    val script = makeScript(analysisResultList.map(_.collimatorCentering.gantryAngleRounded_deg), extendedData.output.outputPK.get)

    val html = Phase2Util.wrapSubProcedure(
      extendedData,
      content,
      "Collimator Centering",
      status,
      Some(script),
      runReq
    )
    val outFile = new File(extendedData.output.dir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status, analysisResultList)

  }
}
