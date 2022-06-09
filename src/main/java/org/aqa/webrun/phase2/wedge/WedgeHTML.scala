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

package org.aqa.webrun.phase2.wedge

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.WedgePoint
import org.aqa.run.ProcedureStatus
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.File
import scala.xml.Elem

object WedgeHTML {

  private val htmlFileName = "Wedge.html"

  val lineColor = new Color(0x6688bb)

  /**
    * Make a tiny summary and link to the detailed report.
    */
  private def makeSummary(status: ProcedureStatus.Value, wedgeDir: File) = {
    val iconImage = if ((status == ProcedureStatus.pass) || (status == ProcedureStatus.done)) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={wedgeDir.getName + "/" + htmlFileName}>
          {WedgeAnalysis.subProcedureName}<br/>
          <img src={iconImage} height="32"/>
        </a>
      </div>
    }
    elem
  }

  private case class ProfilePoint(position: Double, value: Double) {}

  private def profile(
      dicomImage: DicomImage,
      translator: IsoImagePlaneTranslator,
      isTransverse: Boolean,
      attributeList: AttributeList,
      collimatorCenterOfRotation: Point2D.Double
  ): Seq[ProfilePoint] = {

    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head

    val profileList = if (isTransverse) {
      val yTop = translator.iso2Pix(0, -Config.WedgeProfileThickness_mm / 2).getY
      val yBottom = translator.iso2Pix(0, Config.WedgeProfileThickness_mm / 2).getY
      val height = (yBottom - yTop).round.toInt
      val rect = new java.awt.Rectangle(0, yTop.round.toInt, dicomImage.width, height)
      val sums = dicomImage.getSubimage(rect).columnSums

      def indexToProfilePoint(i: Int) = {
        val position = translator.pix2Iso(i, 0).getX + collimatorCenterOfRotation.getX
        val value = sums(i) / height
        ProfilePoint(position, value)
      }
      val profList = sums.indices.map(i => indexToProfilePoint(i))
      profList
    } else {
      val xLeft = translator.iso2Pix(-Config.WedgeProfileThickness_mm / 2, 0).getX
      val xRight = translator.iso2Pix(Config.WedgeProfileThickness_mm / 2, 0).getX
      val width = (xRight - xLeft).round.toInt
      val rect = new java.awt.Rectangle(xLeft.round.toInt, 0, width, dicomImage.height)
      val sums = dicomImage.getSubimage(rect).rowSums

      def indexToProfilePoint(i: Int) = {
        val position = translator.pix2Iso(0, i).getY + collimatorCenterOfRotation.getY
        val value = sums(i) / width
        ProfilePoint(position, value)
      }
      val profList = sums.indices.map(i => indexToProfilePoint(i))
      profList
    }

    profileList.map(p => ProfilePoint(p.position, (p.value * RescaleSlope) + RescaleIntercept))
  }

  private def annotateImage(beamName: String, runReq: RunReq): BufferedImage = {
    val image = runReq.derivedMap(beamName).pixelCorrectedImage.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
    Config.applyWatermark(image)
    val translator = new IsoImagePlaneTranslator(runReq.rtimageMap(beamName))
    Util.addGraticules(image, translator, lineColor)
    image
  }

  /**
    * Create the beam image, write it to disk, and return the HTML to reference and display it.
    */
  private def beamImage(beamName: String, id: String, runReq: RunReq, wedgeDir: File): Elem = {
    val bufferedImage = annotateImage(beamName, runReq)
    val pngFile = new File(wedgeDir, "Wedge_" + WebUtil.stringToUrlSafe(beamName) + ".png")
    Util.writePng(bufferedImage, pngFile)
    <center id={id}><img class="img-responsive" src={pngFile.getName}/></center>
  }

  private def beamToDisplay(
      wedgePoint: WedgePoint,
      runReq: RunReq,
      wedgeDir: File,
      collimatorCenterOfRotation: Point2D.Double
  ): (Elem, String) = {

    val isTransverse = WedgeAnalysis.wedgeOrientationTransverse(wedgePoint.wedgeBeamName, runReq.rtplan)
    val dicomImage = runReq.derivedMap(wedgePoint.wedgeBeamName).pixelCorrectedImage
    val translator = new IsoImagePlaneTranslator(runReq.rtimageMap(wedgePoint.wedgeBeamName))

    val wedgeProfile = profile(dicomImage, translator, isTransverse, runReq.rtimageMap(wedgePoint.wedgeBeamName), collimatorCenterOfRotation)
    val valueChart = new C3Chart(
      xAxisLabel = "Position mm",
      xDataLabel = "Position mm",
      xValueList = wedgeProfile.map(p => p.position),
      yAxisLabels = Seq("Level"),
      yDataLabel = "Level",
      yValues = Seq(wedgeProfile.map(p => p.value))
    )

    val backgroundDerived = runReq.derivedMap(wedgePoint.backgroundBeamName).pixelCorrectedImage // runReq.rtimageMap(wedgePoint.backgroundBeamName)
    val backgroundProfile = profile(backgroundDerived, translator, isTransverse, runReq.derivedMap(wedgePoint.backgroundBeamName).attributeList, collimatorCenterOfRotation)
    val percentProfile = wedgeProfile.zip(backgroundProfile).map(wb => ProfilePoint(wb._1.position, (wb._1.value * 100) / wb._2.value))
    val percentChart =
      new C3Chart(
        xAxisLabel = "Position mm",
        xDataLabel = "Position mm",
        xValueList = percentProfile.map(p => p.position),
        yAxisLabels = Seq("Percent"),
        yDataLabel = "Percent",
        yValues = Seq(percentProfile.map(p => p.value))
      )
    val backgroundChart =
      new C3Chart(
        xAxisLabel = "Position mm",
        xDataLabel = "Position mm",
        xValueList = backgroundProfile.map(p => p.position),
        yAxisLabels = Seq("Level"),
        yDataLabel = "Level",
        yValues = Seq(backgroundProfile.map(p => p.value))
      )

    // Instructions to load the checkbox dynamically.
    val getBaselineHtmlDynamically = {
      val dynUrl = baseUrl + "?" + WedgeUseAsBaseline.loadCheckboxTag + "=" + wedgePoint.wedgePointPK.get
      <div id="DynamicContent1" href={dynUrl}> </div>
    }

    val elem = {
      <div class="row">
        {getBaselineHtmlDynamically}
        <div class="row">
          <div class="col-md-12">
            <h2>
              {wedgePoint.wedgeBeamName + " : "}
              <font color="orange" title="Center value from image">{Util.fmtDbl(wedgePoint.backgroundValue_cu)} CU</font>
              /
              <font color="orange" title="Center value from image as percent of background"> {Util.fmtDbl(wedgePoint.percentOfBackground_pct)} %</font>
            </h2>
          </div>
        </div>
        <div class="row">
          <div class="row">
            <div class="col-md-2">
              <h4>History of Wedge Point</h4>
            </div>
            <div class="col-md-2">
              {C3ChartHistory.htmlHelp()}
            </div>
          </div>
          <div class="row">
            {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + Util.textToId(wedgePoint.wedgeBeamName))}
          </div>
        </div>
        <div class="row">
          <div class="col-md-5">
            <h3>Wedge {wedgePoint.wedgeBeamName}</h3>
            {beamImage(wedgePoint.wedgeBeamName, "wedge", runReq, wedgeDir)}
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3 title="Wedge profile as percent of background">Wedge Factor</h3>
              {percentChart.html}
            </div>
            <div class="row">
              <h3>Wedge Profile</h3>
              {valueChart.html}
            </div>
          </div>
        </div>
        <div class="row">
          <div class="col-md-5">
            <h3>Background {wedgePoint.backgroundBeamName}</h3>
            {beamImage(wedgePoint.backgroundBeamName, "background", runReq, wedgeDir)}
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3>Background Profile</h3>
              {backgroundChart.html}
            </div>
          </div>
        </div>
      </div>
    }

    val zoomScript = """
      $(document).ready(function(){ $('#wedge').zoom(); });
      $(document).ready(function(){ $('#background').zoom(); });
"""
    val js = valueChart.javascript + percentChart.javascript + backgroundChart.javascript + zoomScript
    (elem, js)
  }

  /** Base URL of handler for checkboxes. */
  private val baseUrl = (new WedgeUseAsBaseline).pathOf

  private val respondToBaselineCheckboxScript = {
    s"""
        <script>

          function reloadMe() {
            location.reload();
          }

          var baseUrl='$baseUrl';

          function setBaselineState(checkBox, wedgePointPK) {
            var baselineUrl=baseUrl + '?wedgePointPK=' + wedgePointPK.toString().trim() + '&baseline=' + checkBox.checked.toString();
            // send the request to use or not use the beam as a baseline.  Note that if the user is
            // not authorized, then the request will be silently rejected.
            // alert("wedgePoint checkpoint: " + baselineUrl);  // TODO rm

            var https = new XMLHttpRequest();
            https.open("GET", baselineUrl, true);
            https.send();
            // reload the page to show the changes due to changing the baseline
            setTimeout(reloadMe, 1000);
          }
        </script>
      """.replace("\r", "").stripMargin
  }

  def makeDisplay(extendedData: ExtendedData, status: ProcedureStatus.Value, runReq: RunReq, wedgePointList: Seq[WedgePoint], collimatorCenterOfRotation: Point2D.Double): Elem = {

    val outputDir = extendedData.output.dir
    val wedgeDir = new File(outputDir, "wedge")
    Util.mkdirs(wedgeDir)

    val htmlJs = {
      wedgePointList.map(wp => beamToDisplay(wp, runReq, wedgeDir, collimatorCenterOfRotation))
    }

    val content = {
      <div class="col-md-10 col-md-offset-1">
        {htmlJs.map(ej => ej._1)}
      </div>
    }

    val javascript =
      "<script>\n" + htmlJs.map(ej => ej._2).mkString("\n") + "</script>\n" +
        respondToBaselineCheckboxScript +
        WedgeChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)

    val html = Phase2Util.wrapSubProcedure(extendedData, content, WedgeAnalysis.subProcedureName, status, Some(javascript), runReq)
    val outFile = new File(wedgeDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status, wedgeDir)
  }

}
