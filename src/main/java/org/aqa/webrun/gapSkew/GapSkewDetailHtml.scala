/*
 * Copyright 2022 Regents of the University of Michigan
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

package org.aqa.webrun.gapSkew

import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.GapSkew.EdgeType
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.gapSkew.GapSkewUtil._

import java.io.File
import scala.xml.Elem

object GapSkewDetailHtml {
  def htmlFileName(beamName: String): String = Util.textToId(beamName) + ".html"
}

case class GapSkewDetailHtml(extendedData: ExtendedData, leafSet: LeafSet, runReq: GapSkewRunReq) {

  private val gapSkew = leafSet.gapSkew

  private val dicomHtml = DicomHtml(extendedData, title = leafSet.gapSkew.beamName + " DICOM")

  private val imageUrl: String = FileUtil.replaceInvalidFileNameCharacters(leafSet.gapSkew.beamName, '_').replace(' ', '_') + ".png"

  /**
    * Make a summary table for the beam.
    * @return Summary as HTML.
    */
  private def summaryTable: Elem = {

    def td(d: Double): Elem = {
      <td title={d.toString}>{fmt2(d)}</td>
    }

    def tdAngle(angle: Double): Elem = {
      val title = {
        angle.toString + "   Warning limit: " + Config.GapSkewAngleWarn_deg + "  Fail limit: " + Config.GapSkewAngleFail_deg
      }
      <td style={"background-color:" + statusColor(angle) + ";"} title={title}>{fmt2(angle)}</td>
    }

    def formatEdgeType(edgeType: EdgeType): String = {
      (if (edgeType.isX) "X" else "Y") + edgeType.bank + " " + (if (edgeType.isJaw) "Jaw" else "MLC")
    }

    <table class="table table-bordered">
      <thead>
        <tr>
          <th> Position (mm) </th>
          <th> Skew (deg) </th>
          <th> Left - Right (mm) </th>
        </tr>

        <tr>
          <td style="white-space: nowrap;">Top ({formatEdgeType(gapSkew.topLeftEdgeType)})</td>
          {tdAngle(gapSkew.topHorzSkew_deg)}
          {td(gapSkew.topHorzDelta_mm)}
        </tr>

        <tr>
          <td style="white-space: nowrap;">Top ({formatEdgeType(gapSkew.bottomLeftEdgeType)})</td>
          {tdAngle(gapSkew.bottomHorzSkew_deg)}
          {td(gapSkew.bottomHorzDelta_mm)}
        </tr>

      </thead>
    </table>
  }

  /**
    * Generate the HTML references to the history charts.
    * @return References to charts.
    */
  private def historyCharts(): Elem = {
    <div>
      <div class="row">
        <h4>Angles</h4>
        {C3Chart.html(GapSkewHistoryChart.angleChartIdTag(gapSkew.beamName))}
      </div>
      <div class="row">
        <h4>Offsets</h4>
        {C3Chart.html(GapSkewHistoryChart.offsetChartIdTag(gapSkew.beamName))}
      </div>
    </div>
  }

  private val leafTitle: Elem = {
    val beamTitle = { "Largest skew (rotational) error of top and bottom (0 is ideal) in degrees." + WebUtil.titleNewline + gapSkew.largestHorzSkew_deg.formatted("%20.8f").trim }
    val color = statusColor(gapSkew.largestHorzSkew_deg)
    val heading = <h3 style={s"margin:8px; background-color:$color; border:solid $color 1px; border-radius: 18px; padding: 12px;"} title={beamTitle}> {gapSkew.beamName} </h3>
    heading
  }

  /**
    * Generate detailed HTML content for one RTIMAGE.
    * @return HTML content.
    */
  private def content = {

    val detailTable = new GapSkewHtmlTable(gapSkew, dicomHtml.htmlUrl, imageUrl).detailTable

    <div class="row">
      <div class="col-md-12 col-md-offset-1">
        <div class="row">
          <div class="col-md-4 col-md-offset-4">
            <center> {leafTitle} </center>
          </div>
        </div>

        <div class="row">
          <div class="col-md-4 col-md-offset-4">
            <center> {summaryTable} </center>
          </div>
        </div>

        <div class="row">
          <div class="col-md-8 col-md-offset-2">
            <center>
              {detailTable}
            </center>
          </div>
        </div>

        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {historyCharts()}
          </div>
        </div>
      </div>
    </div>

  }

  /**
    * Generate a detailed HTML web page of this RTIMAGE and write it to disk.
    */
  def writeDetailedHtml(): Unit = {
    val script = GapSkewHistoryRestlet.scriptReference(gapSkew.beamName, extendedData.output.outputPK.get)

    val text = WebUtil.wrapBody(
      content = ExtendedData.wrapExtendedData(extendedData, content, offset = 0),
      pageTitle = gapSkew.beamName + " Gap Skew",
      refresh = None,
      c3 = true,
      runScript = Some(script)
    )

    val file = new File(extendedData.output.dir, GapSkewDetailHtml.htmlFileName(gapSkew.beamName))

    Util.writeFile(file, text)

    Util.writePng(leafSet.image, new File(extendedData.output.dir, imageUrl))

    dicomHtml.makeDicomContent(leafSet.attributeList, Some(imageUrl))
  }

  /**
    * Generate the HTML to show the summary of this RTIMAGE.
    * @return HTML summary.
    */
  def summaryHtml(): Elem = {
    <div class="row" style="margin-top: 40px;">
      <div class="col-md-3"> <center> {leafTitle}<p> </p> <a href={GapSkewDetailHtml.htmlFileName(gapSkew.beamName)}>Details</a> </center> </div>
      <div class="col-md-5">{summaryTable}</div>
    </div>
  }

}
