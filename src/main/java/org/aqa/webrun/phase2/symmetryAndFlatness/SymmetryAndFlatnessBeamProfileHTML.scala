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

package org.aqa.webrun.phase2.symmetryAndFlatness

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.psm.html.PSMHtmlImage
import org.aqa.webrun.psm.PSMUtil

import java.awt.Color
import java.io.File
import scala.xml.Elem

/**
  * Analyze DICOM files for symmetry and flatness.
  */
object SymmetryAndFlatnessBeamProfileHTML extends Logging {

  private def makeContent(
      subDir: File,
      extendedData: ExtendedData,
      result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult,
      runReq: RunReq
  ): (Elem, String) = {

    val graphTransverse = new C3Chart(
      xAxisLabel = "Position mm",
      xDataLabel = "Position mm",
      xValueList = result.transverse_pct.toList,
      yAxisLabels = Seq("Level").toList,
      yDataLabel = "Level",
      yValues = Seq(result.transverseProfile.toList).toList,
      yColorList = Seq(new Color(0x4477bb)).toList
    )

    val graphAxial = new C3Chart(
      xAxisLabel = "Position mm",
      xDataLabel = "Position mm",
      xValueList = result.axial_pct.toList,
      yAxisLabels = Seq("Level").toList,
      yDataLabel = "Level",
      yValues = Seq(result.axialProfile.toList).toList,
      yColorList = Seq(new Color(0x4477bb)).toList
    )

    def psmProcessing: (Seq[Elem], String) = {
      val psm = runReq.getPsm(extendedData.machine.machinePK.get)
      if (result.symmetryAndFlatness.psmImageHash_md5.isEmpty || psm.isEmpty)
        (Seq(), "")
      else {

        val symFlat = result.symmetryAndFlatness

        val wdAl = runReq.rtimageMap(symFlat.beamName)
        val trans = new IsoImagePlaneTranslator(runReq.rtimageMap(symFlat.beamName))

        val ffImg = psm.get.getFloodFieldScaled

        val wdImg = new DicomImage(wdAl).scalePixels(wdAl)

        val ffXwdImg = ffImg.fun2((a, b) => a * b, wdImg)

        val psmImg = new DicomImage(psm.get.dicom).scalePixels(psm.get.dicom)

        val brImg = ffXwdImg.fun2(PSMUtil.funcDiv, psmImg)

        val dir = SymmetryAndFlatnessHTML.makeSubDir(extendedData.output.dir)

        val ffRow = PSMHtmlImage(extendedData, "FF: Flood Field", ffImg, trans, dir = dir, al = Some(psm.get.getFloodFieldDicom))
        val wdRow = PSMHtmlImage(extendedData, "WD: " + symFlat.beamName + " used as Whole Detector", wdImg, trans, dir = dir, al = Some(wdAl))
        val ffXWDRow = PSMHtmlImage(extendedData, symFlat.beamName + " times Flood Field", ffXwdImg, trans, dir = dir)
        val psmRow = PSMHtmlImage(extendedData, "PSM", psmImg, trans, dir = dir, al = Some(psm.get.dicom))
        val brRow = PSMHtmlImage(extendedData, "BR: Beam Response", brImg, trans, dir = dir)

        val elem = {
          <div class="row">
            <div class="col-md-10">
              {WebUtil.showPrecision}
              <table class="table responsive table-bordered" style="margin-top:25px;">
                <thead>
                  <tr>
                    <th title="Click for larger image, larger chart, and metadata.">
                      Image
                    </th>
                    <th>
                      Center Pixels
                    </th>
                    <th>
                      Profiles
                    </th>
                  </tr>
                </thead>
                {ffRow.elem}
                {wdRow.elem}
                {ffXWDRow.elem}
                {psmRow.elem}
                {brRow.elem}
              </table>
            </div>
          </div>
        }

        val js = Seq(ffRow, wdRow, ffXWDRow, psmRow, brRow).map(_.js).mkString("\n")
        (Seq(elem), js)
      }
    }

    val psmProc = psmProcessing

    val psmElem = psmProc._1
    val psmJs = psmProc._2

    val content = {

      val dynamicUrl: String = {
        (new SymmetryAndFlatnessSubHTML).pathOf +
          "?outputPK=" + extendedData.output.outputPK.get +
          "&" + SymmetryAndFlatnessSubHTML.beamNameTag + "=" + result.symmetryAndFlatness.beamName +
          "&" + SymmetryAndFlatnessSubHTML.hasPsmTag + "=" + result.symmetryAndFlatness.psmImageHash_md5.isDefined
      }

      <div class="row">

        <div class="row">
          <div id="DynamicContent1" href={dynamicUrl}> </div>
        </div>

        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            {
        <center id="beamImage"><img class="img-responsive" src={
          WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, result.symmetryAndFlatness.beamName, result.symmetryAndFlatness.psmImageHash_md5.isDefined))
        }/> </center>
      }
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3>Transverse</h3>{graphTransverse.html}
            </div>
            <div class="row">
              <h3>Axial</h3>{graphAxial.html}
            </div>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <div class="row">
              {C3ChartHistory.htmlHelp()}
            </div>
            <div class="row">
              <h2>Transverse Symmetry History</h2>
              {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.transverseSymmetryName))}
            </div>
            <div class="row">
              <h2>Axial Symmetry History</h2>
              {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.axialSymmetryName))}
            </div>
            <div class="row">
              <h2>Flatness History</h2>
              {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.flatnessName))}
            </div>
            <div class="row">
              <h2>Profile Constancy History</h2>
              {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.profileConstancyName))}
            </div>
            <div class="row">
              <h2>EPID CU History</h2>
              {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + "EpidCu")}
            </div>
            <div class="row">
              <h2>EPID Noise History</h2>
              {C3ChartHistory.htmlRef(C3Chart.idTagPrefix + "EpidNoise")}
            </div>
          </div>
          <div class="row">
            <div class="col-md-10 col-md-offset-1">
              {psmElem}
            </div>
          </div>
        </div>
        <p> </p>
      </div>
    }

    val zoomScript = """
    $(document).ready(function(){ $('#beamImage').zoom(); });
"""

    val historyScriptRef = SymmetryAndFlatnessHistoryRestlet.makeReference(result.symmetryAndFlatness.beamName, extendedData.output.outputPK.get, result.symmetryAndFlatness.psmImageHash_md5.isDefined)

    val javascript = "<script>\n" + graphTransverse.javascript + graphAxial.javascript + zoomScript + psmJs + "\n</script>\n" + historyScriptRef
    (content, javascript)
  }

  def makeDisplay(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, runReq: RunReq): Unit = {
    val status = if (result.symmetryAndFlatness.allPass(result.baseline)) ProcedureStatus.pass else ProcedureStatus.fail
    val elemJavascript = makeContent(subDir, extendedData, result, runReq)
    val html = Phase2Util.wrapSubProcedure(extendedData, elemJavascript._1, title = "Symmetry and Flatness " + result.symmetryAndFlatness.beamName, status, Some(elemJavascript._2), runReq.rtimageMap)
    Util.writeBinaryFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, result.symmetryAndFlatness.beamName, result.symmetryAndFlatness.psmImageHash_md5.isDefined), html.getBytes)
  }

}
