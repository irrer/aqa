package org.aqa.webrun.phase2

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

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessBeamProfileHTML extends Logging {

  private def makeContent(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq): (Elem, String) = {
    //    val graphTransverse = graph("Transverse " + result.beamName, result.transverseProfile, result.transverse_mm)
    //    val graphAxial = graph("Axial " + result.beamName, result.axialProfile, result.axial_mm)

    val graphTransverse = new C3Chart(None, None,
      "Position mm", "Position mm", result.transverse_mm, ".4g",
      Seq("Level"), "Level", Seq(result.transverseProfile), ".4g", Seq(new Color(0x4477BB)))

    val graphAxial = new C3Chart(None, None,
      "Position mm", "Position mm", result.axial_mm, ".4g",
      Seq("Level"), "Level", Seq(result.axialProfile), ".4g", Seq(new Color(0x4477BB)))

    def fmt(value: Double): Elem = {
      <small title='Value calculated by this analysis.  Shows up as orange dot in chart.'><font color='orange'> { value.formatted("%9.6f") } </font></small>
    }

    val graphHistory = new SymmetryAndFlatnessBeamHistoryHTML(result.beamName, extendedData)
    val content = {
      <div class="row">
        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            { <img class="img-responsive" src={ WebServer.urlOfResultsFile(SymmetryAndFlatnessHTML.annotatedImageFile(subDir, result.beamName)) }/> }
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
              <h2>Transverse Symmetry History { fmt(result.transverseSymmetry) }</h2>
              { graphHistory.htmlTransverse }
            </div>
            <div class="row">
              <h2>Axial Symmetry History { fmt(result.axialSymmetry) }</h2>
              { graphHistory.htmlAxial }
            </div>
            <div class="row">
              <h2>Flatness History { fmt(result.flatness) }</h2>
              { graphHistory.htmlFlatness }
            </div>
          </div>
        </div>
        <p> </p>
      </div>
    }
    val javascript = "<script>\n" + graphTransverse.javascript + graphAxial.javascript + graphHistory.javascript + "\n</script>\n"
    (content, javascript)
  }

  def makeDisplay(subDir: File, extendedData: ExtendedData, result: SymmetryAndFlatnessAnalysis.SymmetryAndFlatnessBeamResult, status: ProcedureStatus.Value, runReq: RunReq) = {
    val elemJavascript = makeContent(subDir, extendedData, result, status, runReq)
    val html = Phase2Util.wrapSubProcedure(extendedData, elemJavascript._1, "Symmetry and Flatness Beam " + result.beamName, result.status, Some(elemJavascript._2), runReq)
    Util.writeBinaryFile(SymmetryAndFlatnessHTML.beamHtmlFile(subDir, result.beamName), html.getBytes)
  }
}
