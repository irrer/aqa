package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Logging
import org.aqa.db.CollimatorPosition
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateEdge
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point
import edu.umro.ImageUtil.ImageText
import java.io.File
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.C3Chart
import java.text.Format
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.Phase2Util

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessBeamProfileHTML extends Logging {

  private def makeContent(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq): (Elem, String) = {

    val graphTransverse = new C3Chart(None, None,
      "Position mm", "Position mm", result.transverse_pct, ".4g",
      Seq("Level"), "Level", Seq(result.transverseProfile), ".4g", Seq(new Color(0x4477BB)))

    val graphAxial = new C3Chart(None, None,
      "Position mm", "Position mm", result.axial_pct, ".4g",
      Seq("Level"), "Level", Seq(result.axialProfile), ".4g", Seq(new Color(0x4477BB)))

    def fmt(value: Double): Elem = {
      <small title='Value calculated by this analysis.  Shows up as orange dot in chart.'><font color='orange'> { value.formatted("%9.6f") } </font></small>
    }

    def resultTable: Elem = {
      <div style="margin:20px;">
        <center><h3>Results</h3></center>
        <table class="table table-bordered" title={ "Results of this analysis and baseline values" + WebUtil.titleNewline + "for comparison.  All values are in percent." }>
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
            <td>{ Util.fmtDbl(result.transverseSymmetry) }</td>
            <td>{ Util.fmtDbl(result.axialSymmetry) }</td>
            <td>{ Util.fmtDbl(result.flatness) }</td>
            <td>{ Util.fmtDbl(result.profileConstancy) }</td>
          </tr>
          <tr>
            <td>Baseline</td>
            <td>{ Util.fmtDbl(result.transverseSymmetryBaseline) }</td>
            <td>{ Util.fmtDbl(result.axialSymmetryBaseline) }</td>
            <td>{ Util.fmtDbl(result.flatnessBaseline) }</td>
            <td>{ Util.fmtDbl(result.profileConstancyBaseline) }</td>
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
            <td>{ Util.fmtDbl(result.pointSet.top) }</td>
            <td>{ Util.fmtDbl(result.pointSet.bottom) }</td>
            <td>{ Util.fmtDbl(result.pointSet.left) }</td>
            <td>{ Util.fmtDbl(result.pointSet.right) }</td>
            <td>{ Util.fmtDbl(result.pointSet.center) }</td>
          </tr>
          <tr>
            <td>Baseline</td>
            <td>{ Util.fmtDbl(result.baselinePointSet.top) }</td>
            <td>{ Util.fmtDbl(result.baselinePointSet.bottom) }</td>
            <td>{ Util.fmtDbl(result.baselinePointSet.left) }</td>
            <td>{ Util.fmtDbl(result.baselinePointSet.right) }</td>
            <td>{ Util.fmtDbl(result.baselinePointSet.center) }</td>
          </tr>
        </table>
      </div>
    }

    // val graphHistory = new SymmetryAndFlatnessBeamHistoryHTML(result.beamName, extendedData.output.outputPK.get)  TODO rm
    val content = {
      <div class="row">
        <div class="row">
          <div class="col-md-4 col-md-offset-4">
            { resultTable }
          </div>
        </div>
        <div class="row">
          <div class="col-md-4 col-md-offset-4">
            { inputTable }
          </div>
        </div>
        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            { <center id="beamImage"><img class="img-responsive" src={ WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, result.beamName)) }/> </center> }
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3>Transverse</h3>{ graphTransverse.html }
            </div>
            <div class="row">
              <h3>Axial</h3>{ graphAxial.html }
            </div>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <div class="row">
              <h2>Transverse Symmetry History</h2>
              { C3Chart.html(C3Chart.idTagPrefix + Phase2Util.textToId(SymmetryAndFlatnessAnalysis.transverseSymmetryName)) }
            </div>
            <div class="row">
              <h2>Axial Symmetry History</h2>
              { C3Chart.html(C3Chart.idTagPrefix + Phase2Util.textToId(SymmetryAndFlatnessAnalysis.axialSymmetryName)) }
            </div>
            <div class="row">
              <h2>Flatness History</h2>
              { C3Chart.html(C3Chart.idTagPrefix + Phase2Util.textToId(SymmetryAndFlatnessAnalysis.flatnessName)) }
            </div>
            <div class="row">
              <h2>Profile Constancy History</h2>
              { C3Chart.html(C3Chart.idTagPrefix + Phase2Util.textToId(SymmetryAndFlatnessAnalysis.profileConstancyName)) }
            </div>
          </div>
        </div>
        <p> </p>
      </div>
    }

    val zoomScript = """
    $(document).ready(function(){ $('#beamImage').zoom(); });
"""

    val historyScriptRef = SymmetryAndFlatnessHistoryRestlet.makeReference(result.beamName, extendedData.output.outputPK.get)

    val javascript = "<script>\n" + graphTransverse.javascript + graphAxial.javascript + zoomScript + "\n</script>\n" + historyScriptRef
    (content, javascript)
  }

  def makeDisplay(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq) = {
    val elemJavascript = makeContent(subDir, extendedData, result, status, runReq)
    val html = Phase2Util.wrapSubProcedure(extendedData, elemJavascript._1, "Symmetry and Flatness " + result.beamName, result.status, Some(elemJavascript._2), runReq)
    Util.writeBinaryFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, result.beamName), html.getBytes)
  }
}
