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

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq

import java.io.File
import scala.xml.Elem

/**
  * Make HTML for symmetry and flatness.
  */
object SymmetryAndFlatnessHTML extends Logging {

  private val htmlFileName = "SymmetryAndFlatness.html"
  private val subDirName = "SymmetryAndFlatness"

  /**
    * Make the directory to contain symmetry, flatness, and constancy HTML report.
    *
    * @param outputDir Main output directory.
    * @return Sub-directory.
    */
  def makeSubDir(outputDir: File): File = {
    val subDir = new File(outputDir, subDirName)
    Util.mkdirs(subDir)
    subDir
  }

  def annotatedImageFile(subDir: File, beamName: String): File = {
    val fileName = "Sym_Flat_" + WebUtil.stringToUrlSafe(beamName) + ".png"
    new File(subDir, fileName)
  }

  /**
    * Get the file of the HTML for the given beam.
    */
  def beamHtmlFile(subDir: File, beamName: String): File = {
    val fileName = WebUtil.stringToUrlSafe(beamName) + ".html"
    new File(subDir, fileName)
  }

  private def summary(mainHtmlFile: File, resultListSize: Int, status: ProcedureStatus.Value) = {
    val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={WebServer.urlOfResultsFile(mainHtmlFile)}>
          Symmetry
          &amp;
          Flatness Beams:
          {resultListSize}<br/>
          <img src={iconImage} height="32"/>
        </a>
      </div>
    }
    elem
  }

  def makeDisplay(extendedData: ExtendedData, resultList: List[SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult], status: ProcedureStatus.Value, runReq: RunReq): Elem = {
    val subDir = makeSubDir(extendedData.output.dir)
    val mainHtmlFile = new File(subDir, htmlFileName)
    resultList.par.foreach(rb => Util.writePng(rb.annotatedImage, annotatedImageFile(subDir, rb.symmetryAndFlatness.beamName)))

    val dynamicContent = {
      val url = (new SymmetryAndFlatnessSubHTML).pathOf + "?outputPK=" + extendedData.output.outputPK.get

      <div class="col-md-6 col-md-offset-3">
        <div id="DynamicContent1" href={url}> </div>
      </div>
    }

    val runScript = {
      """
        <script>

          function reloadMe() {
            location.reload();
          }

          var baseUrl='/admin/SymmetryAndFlatnessSubHTML';

          function setBaselineState(checkBox, symFlatPK) {
            var baselineUrl=baseUrl + '?symFlatPK=' + symFlatPK.toString().trim() + '&baseline=' + checkBox.checked.toString();
            // send the request to use or not use the beam as a baseline.  Note that if the user is
            // not authorized, then the request will be silently rejected.
            // alert("Sym Flat " + baselineUrl);  // TODO rm

            var xhttp = new XMLHttpRequest();
            xhttp.open("GET", baselineUrl, true);
            xhttp.send();
            // reload the page to show the changes due to changing the baseline
            setTimeout(reloadMe, 1000);
          }
        </script>
      """.replace("\r", "")
    }
    val html = Phase2Util.wrapSubProcedure(extendedData, dynamicContent, title = "Symmetry and Flatness", status, Some(runScript), runReq)
    Util.writeFile(mainHtmlFile, html)

    resultList.foreach(rb => SymmetryAndFlatnessBeamProfileHTML.makeDisplay(subDir, extendedData, rb, runReq))

    summary(mainHtmlFile, resultList.size, status)
  }

}
