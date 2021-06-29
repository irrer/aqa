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

import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory
import org.aqa.web.WebServer
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq

import java.awt.Color
import java.io.File
import scala.collection.Seq
import scala.xml.Elem

/**
  * Analyze DICOM files for symmetry and flatness.
  */
object SymmetryAndFlatnessBeamProfileHTML extends Logging {

  private def makeContent(
      subDir: File,
      extendedData: ExtendedData,
      result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult
  ): (Elem, String) = {

    val graphTransverse = new C3Chart(
      xAxisLabel = "Position mm",
      xDataLabel = "Position mm",
      xValueList = result.transverse_pct,
      yAxisLabels = Seq("Level"),
      yDataLabel = "Level",
      yValues = Seq(result.transverseProfile),
      yColorList = Seq(new Color(0x4477bb))
    )

    val graphAxial = new C3Chart(
      xAxisLabel = "Position mm",
      xDataLabel = "Position mm",
      xValueList = result.axial_pct,
      yAxisLabels = Seq("Level"),
      yDataLabel = "Level",
      yValues = Seq(result.axialProfile),
      yColorList = Seq(new Color(0x4477bb))
    )

    // val graphHistory = new SymmetryAndFlatnessBeamHistoryHTML(result.beamName, extendedData.output.outputPK.get)  TODO rm
    val content = {

      val dynamicUrl: String = {
        (new SymmetryAndFlatnessSubHTML).pathOf + "?outputPK=" + extendedData.output.outputPK.get + "&" + SymmetryAndFlatnessSubHTML.beamNameTag + "=" + result.symmetryAndFlatness.beamName
      }

      <div class="row">

        <div class="row">
          <div id="DynamicContent1" href={dynamicUrl}> </div>
        </div>

        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            {<center id="beamImage"><img class="img-responsive" src={WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, result.symmetryAndFlatness.beamName))}/> </center>}
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
          </div>
        </div>
        <p> </p>
      </div>
    }

    val zoomScript = """
    $(document).ready(function(){ $('#beamImage').zoom(); });
"""

    val historyScriptRef = SymmetryAndFlatnessHistoryRestlet.makeReference(result.symmetryAndFlatness.beamName, extendedData.output.outputPK.get)

    val javascript = "<script>\n" + graphTransverse.javascript + graphAxial.javascript + zoomScript + "\n</script>\n" + historyScriptRef
    (content, javascript)
  }

  def makeDisplay(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, runReq: RunReq): Unit = {
    val status = if (result.symmetryAndFlatness.allPass(result.baseline)) ProcedureStatus.pass else ProcedureStatus.fail
    val elemJavascript = makeContent(subDir, extendedData, result)
    val html = Phase2Util.wrapSubProcedure(extendedData, elemJavascript._1, title = "Symmetry and Flatness " + result.symmetryAndFlatness.beamName, status, Some(elemJavascript._2), runReq)
    Util.writeBinaryFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, result.symmetryAndFlatness.beamName), html.getBytes)
  }
}
