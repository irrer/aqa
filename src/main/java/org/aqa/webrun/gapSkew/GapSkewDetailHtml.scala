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

  private def td(d: Double): Elem = {
    <td title={d.toString}>{fmt2(d)}</td>
  }

  private def tdAngle(angle: Double): Elem = {
    val title = {
      angle.toString + "   Warning limit: " + Config.GapSkewAngleWarn_deg + "  Fail limit: " + Config.GapSkewAngleFail_deg
    }
    <td style={"background-color:" + statusColor(angle) + ";"} title={title}>{fmt2(angle)}</td>
  }

  private def formatEdgeType(edgeType: EdgeType): String = {
    (if (edgeType.isX) "X" else "Y") + edgeType.bank + " " + (if (edgeType.isJaw) "Jaw" else "MLC")
  }

  /**
    * Make a summary table for the beam.
    * @return Summary as HTML.
    */
  private def summaryTable: Elem = {
    <table class="table table-bordered">
      <thead>
        <tr>
          <th> Position (mm) </th>
          <th> Skew (deg) </th>
          <th title="Change in mm of measurement: Right - Left "> Delta (mm) </th>
        </tr>
      </thead>

        <tr>
          <td style="white-space: nowrap;">{formatEdgeType(gapSkew.topLeftEdgeType)} (top)</td>
          {tdAngle(gapSkew.topHorzSkew_deg)}
          {td(gapSkew.topHorzDelta_mm)}
        </tr>

        <tr>
          <td style="white-space: nowrap;">{formatEdgeType(gapSkew.bottomLeftEdgeType)} (bottom)</td>
          {tdAngle(gapSkew.bottomHorzSkew_deg)}
          {td(gapSkew.bottomHorzDelta_mm)}
        </tr>

    </table>
  }

  private def detailedTable: Elem = {
    <table class="table table-bordered">
      <thead>
        <tr>
          <th> Position (mm) </th>
          <th> Skew (deg) </th>
          <th title="Change in mm of measurement: Right - Left"> Delta (mm) </th>

          <th>Planned (mm)</th>
          <th>Left (mm)</th>
          <th>Left Delta (mm)</th>
          <th>Right (mm)</th>
          <th>Right Delta (mm)</th>

        </tr>
      </thead>

      <tr>
        <td style="white-space: nowrap;">{formatEdgeType(gapSkew.topLeftEdgeType)} (top)</td>
        {tdAngle(gapSkew.topHorzSkew_deg)}
        {td(gapSkew.topHorzDelta_mm)}

        {td(gapSkew.topLeftPlanned_mm.get)}
        {td(gapSkew.topLeftValue_mm.get)}
        {td(gapSkew.topLeftHorzDelta_mm)}
        {td(gapSkew.topRightValue_mm.get)}
        {td(gapSkew.topRightHorzDelta_mm)}

      </tr>

      <tr>
        <td style="white-space: nowrap;">{formatEdgeType(gapSkew.bottomLeftEdgeType)} (bottom)</td>
        {tdAngle(gapSkew.bottomHorzSkew_deg)}
        {td(gapSkew.bottomHorzDelta_mm)}

        {td(gapSkew.bottomLeftPlanned_mm.get)}
        {td(gapSkew.bottomLeftValue_mm.get)}
        {td(gapSkew.bottomLeftHorzDelta_mm)}
        {td(gapSkew.bottomRightValue_mm.get)}
        {td(gapSkew.bottomRightHorzDelta_mm)}

      </tr>

    </table>
  }

  private def verticalTable: Elem = {
    <table class="table table-bordered">
      <thead>
        <tr>
          <th> </th>
          <th> Left </th>
          <th> Right </th>
        </tr>
      </thead>

      <tr>
        <td style="white-space: nowrap;">Vertical Delta (mm) </td>
        {td(gapSkew.leftDeltaSeparationOfHorzEdges_mm)}
        {td(gapSkew.rightDeltaSeparationOfHorzEdges_mm)}
      </tr>

      <tr>
        <td style="white-space: nowrap;">Vertical Measured (mm) </td>
        {td(gapSkew.leftSeparationOfHorzEdges_mm)}
        {td(gapSkew.rightSeparationOfHorzEdges_mm)}
      </tr>

      <tr>
        <td style="white-space: nowrap;">Vertical Planned (mm) </td>
        {td(gapSkew.plannedEdgeSeparation_mm)}
        {td(gapSkew.plannedEdgeSeparation_mm)}
      </tr>

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
    val color = statusColor(gapSkew.largestHorzSkew_deg)
    val heading = <h3 style={s"margin:8px; background-color:$color; border:solid $color 1px; border-radius: 18px; padding: 12px;"} title={"Click for details"}> {gapSkew.beamName} </h3>
    heading
  }

  private val detailUrl: String = GapSkewDetailHtml.htmlFileName(gapSkew.beamName)

  /**
    * Generate detailed HTML content for one RTIMAGE.
    * @return HTML content.
    */
  private def content = {

    val spatialTable = new GapSkewHtmlTable(gapSkew, dicomHtml.htmlUrl, imageUrl).detailTable

    <div class="row">
      <div class="col-md-12 col-md-offset-1">
        <div class="row">
          <div class="col-md-4 col-md-offset-4">
            <center> {leafTitle} </center>
          </div>
        </div>

        <div class="row">
          <div class="col-md-6 col-md-offset-3">
            <center>
              {detailedTable}
            </center>
          </div>
        </div>

        <div class="row">
          <div class="col-md-4 col-md-offset-4">
            <center>
              <h4>Vertical Measurements</h4>
              {verticalTable}
            </center>
          </div>
        </div>

        <div class="row">
          <div class="col-md-8 col-md-offset-2">
            <center style="margin-top: 40px; margin-bottom: 40px;">
              {spatialTable}
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
      <div class="col-md-3">
        <a href={detailUrl} title="Click for details.">
          <center style="padding: 24px;">
            {leafTitle}<p> </p>
          </center>
        </a></div>
      <div class="col-md-5">{summaryTable}</div>
      <div class="col-md-2">
        <a href={detailUrl} title="Click for details.">
          <img class="img-responsive fit-image" src={imageUrl} style="width:384px;"/>
        </a>
      </div>
    </div>
  }

}
