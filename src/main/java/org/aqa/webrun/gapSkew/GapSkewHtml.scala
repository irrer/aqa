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

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

import java.awt.Color
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem

object GapSkewHtml {

  def makeDisplay(extendedData: ExtendedData, runReq: GapSkewRunReq, leafSetSeq: Seq[LeafSet], procedureStatus: ProcedureStatus.Value): Unit = {

    def makeDicomContent(al: dicom.AttributeList, title: String): String = {
      val baseFileName = title.replaceAll("[^a-zA-Z0-9_]", "_")
      val htmlFileName = "DICOM_" + baseFileName + ".html"
      val dicomFileName = "DICOM_" + baseFileName + ".dcm"

      def fileOf(name: String) = new File(extendedData.output.dir, name)
      DicomUtil.writeAttributeListToFile(al, fileOf(dicomFileName), "AQA")
      val content = {
        <div>
          <h2>{title}</h2>
          <a href={dicomFileName}>Download DICOM</a>
          <pre>
            {WebUtil.nl + DicomUtil.attributeListToString(al)}
          </pre>
        </div>
      }

      val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), title)
      Util.writeFile(fileOf(htmlFileName), text)

      htmlFileName
    }
    def beamNameOf(leafSet: LeafSet): String = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, leafSet.attributeList).get

    def imageFileOf(leafSet: LeafSet): File = {
      val pngFileName = FileUtil.replaceInvalidFileNameCharacters(leafSet.beamName, '_').replace(' ', '_') + ".png"
      new File(extendedData.output.dir, pngFileName)
    }

    // accumulate all of the chart javascript here
    val chartJavascript = ArrayBuffer[String]()

    /**
      * Make a chart that shows the profile of the beam at the edge.  Also put the javascript in the common buffer.
      * @param leaf Leaf to show.
      * @return HTML reference to chart.
      */
    def makeChart(leaf: Leaf): Elem = {

      val chart = new C3Chart(
        // width = Some(400),
        height = Some(300),
        xAxisLabel = "Position",
        xDataLabel = "cm",
        xValueList = leaf.profile.map(_.y_mm),
        yAxisLabels = Seq("CU"),
        yDataLabel = "CU",
        yValues = Seq(leaf.profile.map(_.cu)),
        //yFormat = ".0f",
        yFormat = ".8gf",
        yColorList = Seq(new Color(0, 255, 255))
      )

      chartJavascript += chart.javascript
      chart.html
    }

    def leafSetToHtml(leafSet: LeafSet): Elem = {

      val pngFile = imageFileOf(leafSet)
      Util.writePng(leafSet.image, pngFile)

      def td(d: Double): Elem = {
        <td title={d.toString}>{d.formatted("%8.2f").trim}</td>
      }

      val skewTable = {
        // @formatter:off
        <table class="table table-bordered">
          <thead>
            <tr>
              <th>
                Position (mm)
              </th>
              <th>
                Left - Right (mm)
              </th>
              <th>
                Left (mm)
              </th>
              <th>
                Right (mm)
              </th>
              <th>
                Planned (mm)
              </th>
            </tr>

            <tr>
              <td>
                Top
              </td>{td(leafSet.topLeft.position_mm - leafSet.topRight.position_mm)}{td(leafSet.topLeft.position_mm)}{td(leafSet.topRight.position_mm)}{td(leafSet.leafPositionRtplanTop_mm)}
            </tr>

            <tr>
              <td>
                Bottom
              </td>{td(leafSet.bottomLeft.position_mm - leafSet.bottomRight.position_mm)}{td(leafSet.bottomLeft.position_mm)}{td(leafSet.bottomRight.position_mm)}{td(leafSet.leafPositionRtplanBottom_mm)}
            </tr>

          </thead>
        </table>
      // @formatter:on
      }

      val offsetTable = {
        // @formatter:off
          <table class="table table-bordered">
          <thead>
            <tr>
              <th>
                Position (mm)
              </th>
              <th>
                Top - Bottom - <br/> Planned (mm)
              </th>
              <th>
                Top (mm)
              </th>
              <th>
                Bottom (mm)
              </th>
            </tr>

            <tr>
              <td>
                Left
              </td>
                {td(leafSet.topLeft.position_mm - leafSet.bottomLeft.position_mm - (leafSet.leafPositionRtplanTop_mm - leafSet.leafPositionRtplanBottom_mm))}
                {td(leafSet.topLeft.position_mm)}
                {td(leafSet.topRight.position_mm)}
            </tr>
          
            <tr>
              <td>
                Right
              </td>
              {td(leafSet.bottomRight.position_mm - leafSet.topRight.position_mm + (leafSet.leafPositionRtplanTop_mm - leafSet.leafPositionRtplanBottom_mm))}
              {td(leafSet.bottomLeft.position_mm)}
              {td(leafSet.bottomRight.position_mm)}
            </tr>
          
          </thead>
        </table>
      // @formatter:on
      }

      val htmlName = makeDicomContent(runReq.rtimageMap(leafSet.beamName), leafSet.beamName)

      def dicomLink = {
        <a href={htmlName}>View DICOM</a>
      }

      val edgeChartList: Elem = {
        <div>
        <h3>Edge Profiles</h3>
        <ul class="nav nav-tabs">
          <li class="active">
            <a data-toggle="tab" href="#topLeft">Top Left {Util.fmtDbl(leafSet.topLeft.position_mm)}</a>
          </li>
          <li>
            <a data-toggle="tab" href="#topRight">Top Right {Util.fmtDbl(leafSet.topRight.position_mm)}</a>
          </li>
          <li>
            <a data-toggle="tab" href="#bottomLeft">Bottom Left {Util.fmtDbl(leafSet.bottomLeft.position_mm)}</a>
          </li>
          <li>
            <a data-toggle="tab" href="#bottomRight">Bottom Right {Util.fmtDbl(leafSet.bottomRight.position_mm)}</a>
          </li>
        </ul>

        <div class="tab-content">
          <div id="topLeft" class="tab-pane fade in active">
            <h3>Top Left</h3>
            {makeChart(leafSet.topLeft)}
          </div>
          <div id="topRight" class="tab-pane fade">
              <h3>Top Right</h3>
            {makeChart(leafSet.topRight)}
          </div>
          <div id="bottomLeft" class="tab-pane fade">
            <h3>Bottom Left</h3>
            {makeChart(leafSet.bottomLeft)}
          </div>
          <div id="bottomRight" class="tab-pane fade">
            <h3>Bottom Right</h3>
            {makeChart(leafSet.bottomRight)}
          </div>
        </div>
      </div>
      }

      val content = {
        <div class="row">
          <hr/>
          <div class="col-md-6">
            <center>
            <h3 style="margin:20px;">
              {leafSet.beamName}
            </h3>
              {skewTable}
              <p></p>
              {offsetTable}
              {dicomLink}
            </center>
          </div>
          <div class="col-md-6">
            <center>
            <a href={pngFile.getName} style="margin:8px;">
              Click image for larger view.
              <div class="zoom" id={pngFile.getName.dropRight(4)}>
                <img height="400;" src={pngFile.getName}/>
              </div>
            </a>
            </center>
          </div>
          {edgeChartList}
        </div>
      }

      content
    }

    val rtplanName = makeDicomContent(runReq.rtplan, "RTPLAN")

    /**
      * Make an HTML reference to the RTPLAN so it can be viewed.
      * @return Link to rtplan.
      */
    def rtplanReference(): Elem = {
      <a href={rtplanName}>View RTPLAN</a>
    }
    def makeScript() = {
      def toZoom(leafSet: LeafSet): String = {
        "$(document).ready(function(){ $('#" + imageFileOf(leafSet).getName.dropRight(4) + "').zoom(); });"
      }
      "\n<script>\n" +
        leafSetSeq.map(toZoom).mkString("\n\n") +
        chartJavascript.mkString("\n//-------------------------------------------\n") +
        "\n</script>\n"
    }

    val content: Elem = {
      <div class="row">
        <div class="col-md-8 col-md-offset-2">
        {rtplanReference()}
          {leafSetSeq.sortBy(beamNameOf).map(leafSetToHtml)}
        </div>
      </div>
    }

    val contentWithHeader = ExtendedData.wrapExtendedData(extendedData, content)
    val text = WebUtil.wrapBody(contentWithHeader, "Leaf Gap and Skew", refresh = None, c3 = true, runScript = Some(makeScript()))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)
    if (true) {
      val img = new DicomImage(runReq.rtimageMap.head._2)
      val text = img.pixelsToText
      val file = new File("""D:\tmp\pix.txt""")
      Util.writeFile(file, text)
      println("wrote to file " + file.getAbsolutePath)
    }
  }

}
