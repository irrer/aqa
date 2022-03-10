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

import org.aqa.Config
import org.aqa.Util
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.gapSkew.GapSkewUtil._

import scala.xml.Elem

case class LeafSetHtml(extendedData: ExtendedData, leafSet: LeafSet, runReq: GapSkewRunReq) {

  val charts = new GapSkewHistoryChart(extendedData, leafSet)

  def leafSetToHtml: Elem = {

    val pngFile = GapSkewHtml.imageFileOf(leafSet, extendedData.output.dir)
    Util.writePng(leafSet.image, pngFile)

    def td(d: Double): Elem = {
      <td title={d.toString}>{fmt2(d)}</td>
    }

    def tdAngle(angle: Double): Elem = {
      val title = {
        angle.toString + "   Warning limit: " + Config.GapSkewAngleWarn_deg + "  Fail limit: " + Config.GapSkewAngleFail_deg
      }

      val style = s"background-color:${beamColor(angle)};"

      <td title={title} style={style}>{fmt2(angle)}</td>
    }

    val skewTable = {
      // @formatter:off
      <table class="table table-bordered">
        <thead>
          <tr>
            <th> Position (mm) </th>
            <th> Skew (deg) </th>
            <th> Left - Right (mm) </th>
            <th> Left (mm) </th>
            <th> Right (mm) </th>
            <th> Planned (mm) </th>
          </tr>

          <tr>
            <td style="white-space: nowrap;">Top ({leafSet.gapSkew.topEdgeType})</td>
            {tdAngle(leafSet.gapSkew.topAngle_deg)}
            {td(leafSet.topLeft.yPosition_mm - leafSet.topRight.yPosition_mm)}
            {td(leafSet.topLeft.yPosition_mm)}
            {td(leafSet.topRight.yPosition_mm)}
            {td(leafSet.leafPositionRtplanTop_mm)}
          </tr>

          <tr>
            <td style="white-space: nowrap;">Bottom ({leafSet.gapSkew.topEdgeType})</td>
            {tdAngle(leafSet.gapSkew.bottomAngle_deg)}
            {td(leafSet.bottomLeft.yPosition_mm - leafSet.bottomRight.yPosition_mm)}
            {td(leafSet.bottomLeft.yPosition_mm)}
            {td(leafSet.bottomRight.yPosition_mm)}
            {td(leafSet.leafPositionRtplanBottom_mm)}
          </tr>

        </thead>
      </table>
      // @formatter:on
    }

    def offsetTable = {
      // @formatter:off
      <table class="table table-bordered">
        <thead>
          <tr>
            <th>
              Position (mm)
            </th>
            <th>
              Planned - Measured (mm)
            </th>
            <th>
              Left (mm)
            </th>
            <th>
              Right (mm)
            </th>
          </tr>

          <tr>
            <td>
              Top
            </td>
            {td((leafSet.leafPositionRtplanTop_mm - leafSet.leafPositionRtplanBottom_mm) - (leafSet.topLeft.yPosition_mm - leafSet.bottomLeft.yPosition_mm) )}
            {td(leafSet.topLeft.yPosition_mm)}
            {td(leafSet.topRight.yPosition_mm)}
          </tr>

          <tr>
            <td>
              Bottom
            </td>
            {td((leafSet.leafPositionRtplanTop_mm - leafSet.leafPositionRtplanBottom_mm) - (leafSet.topLeft.yPosition_mm - leafSet.bottomLeft.yPosition_mm) )}
            {td(leafSet.bottomLeft.yPosition_mm)}
            {td(leafSet.bottomRight.yPosition_mm)}
          </tr>

        </thead>
      </table>
      // @formatter:on
    }

    def htmlName = DicomHtml(extendedData).makeDicomContent(runReq.rtimageMap(leafSet.beamName), leafSet.beamName, Some(GapSkewHtml.imageFileOf(leafSet, extendedData.output.dir).getName))

    def historyCharts(): Elem = {
      <div>
        <div class="row">
          <h4>Angles</h4>
          {C3Chart.html(charts.angleChartIdTag)}
        </div>
        <div class="row">
          <h4>Offsets</h4>
          {C3Chart.html(charts.offsetChartIdTag)}
        </div>
      </div>
    }

    def content = {

      val beamText = leafSet.beamName + " " + WebUtil.nbsp + " " + WebUtil.nbsp + " " + WebUtil.nbsp + " Max Skew: " + fmt2(leafSet.gapSkew.largestAngleError_deg) + " deg"

      val beamTitle = { "Largest skew (rotational) error of top and bottom (0 is ideal) in degrees." + WebUtil.titleNewline + leafSet.gapSkew.largestAngleError_deg.formatted("%20.8f").trim }

      val color = beamColor(leafSet.gapSkew.largestAngleError_deg)

      val tables = {
        <center>
            <h3 style={s"margin:8px; background-color:$color; border:solid $color 1px; border-radius: 18px; padding: 12px;"} title={beamTitle}> {beamText} </h3>
          {skewTable}
          <p></p>
          {offsetTable}
        </center>
      }

      val image = {
        <center>
          <a href={htmlName} style="margin:8px;">
            Click to view larger image and metadata
            <img class="img-responsive fit-image" src={pngFile.getName}/>
          </a>
        </center>
      }

      val container = {
        <div>
          <div class="row">
            <hr/>
            <div class="col-md-6">
              {tables}
            </div>
            <div class="col-md-6">
              {image}
            </div>
          </div>
          <div class="row">
            {historyCharts()}
          </div>
        </div>
      }

      container
    }

    content

  }
}
