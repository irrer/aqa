package org.aqa.webrun.phase2.symmetryAndFlatness

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.web.C3Chart
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
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
      result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult,
      status: ProcedureStatus.Value,
      runReq: RunReq
  ): (Elem, String) = {

    val graphTransverse = new C3Chart(None, None, "Position mm", "Position mm", result.transverse_pct, ".4g", Seq("Level"), "Level", Seq(result.transverseProfile), ".4g", Seq(new Color(0x4477bb)))

    val graphAxial = new C3Chart(None, None, "Position mm", "Position mm", result.axial_pct, ".4g", Seq("Level"), "Level", Seq(result.axialProfile), ".4g", Seq(new Color(0x4477bb)))

    def fmt(value: Double): Elem = {
      <small title='Value calculated by this analysis.  Shows up as orange dot in chart.'><font color='orange'> {value.formatted("%9.6f")} </font></small>
    }

    def resultTable: Elem = {
      Trace.trace("making results table")
      <div style="margin:20px;">
        <center><h3>Results</h3></center>
        <table class="table table-bordered" title={"Results of this analysis and baseline values" + WebUtil.titleNewline + "for comparison.  All values are in percent."}>
          <thead>
            <tr>
              <th>Source</th>
              <th>Transverse Symmetry %</th>
              <th>Axial Symmetry %</th>
              <th>Flatness %</th>
              <th>Profile Constancy %</th>
            </tr>
          </thead>
          <tr>
            <td>Analysis</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.transverseSymmetry)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.axialSymmetry)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.flatness)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.profileConstancy(result.baseline))}</td>
          </tr>
          <tr>
            <td>Baseline</td>
            <td>{Util.fmtDbl(result.baseline.transverseSymmetry)}</td>
            <td>{Util.fmtDbl(result.baseline.axialSymmetry)}</td>
            <td>{Util.fmtDbl(result.baseline.flatness)}</td>
            <td>{Util.fmtDbl(result.baseline.profileConstancy(result.baseline))}</td>
          </tr>
        </table>
      </div>
    }

    def inputTable: Elem = {
      <div style="margin:20px;">
        <center><h3>Inputs</h3></center>
        <table class="table table-bordered" title="Input values from this data set and from baseline.">
          <thead>
            <tr>
              <th>Source</th>
              <th>Top CU</th>
              <th>Bottom CU</th>
              <th>Left CU</th>
              <th>Right CU</th>
              <th>Center CU</th>
            </tr>
          </thead>
          <tr>
            <td>Analysis</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.top_cu)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.bottom_cu)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.left_cu)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.right_cu)}</td>
            <td>{Util.fmtDbl(result.symmetryAndFlatness.center_cu)}</td>
          </tr>
          <tr>
            <td>Baseline</td>
            <td>{Util.fmtDbl(result.baseline.top_cu)}</td>
            <td>{Util.fmtDbl(result.baseline.bottom_cu)}</td>
            <td>{Util.fmtDbl(result.baseline.left_cu)}</td>
            <td>{Util.fmtDbl(result.baseline.right_cu)}</td>
            <td>{Util.fmtDbl(result.baseline.center_cu)}</td>
          </tr>
        </table>
      </div>
    }

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
              {C3Chart.html(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.transverseSymmetryName))}
            </div>
            <div class="row">
              <h2>Axial Symmetry History</h2>
              {C3Chart.html(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.axialSymmetryName))}
            </div>
            <div class="row">
              <h2>Flatness History</h2>
              {C3Chart.html(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.flatnessName))}
            </div>
            <div class="row">
              <h2>Profile Constancy History</h2>
              {C3Chart.html(C3Chart.idTagPrefix + Util.textToId(SymmetryAndFlatnessAnalysis.profileConstancyName))}
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

  def makeDisplay(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq) = {
    val status = if (result.symmetryAndFlatness.allPass(result.baseline)) ProcedureStatus.pass else ProcedureStatus.fail
    val elemJavascript = makeContent(subDir, extendedData, result, status, runReq)
    val html = Phase2Util.wrapSubProcedure(extendedData, elemJavascript._1, title = "Symmetry and Flatness " + result.symmetryAndFlatness.beamName, status, Some(elemJavascript._2), runReq)
    Util.writeBinaryFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, result.symmetryAndFlatness.beamName), html.getBytes)
  }
}
