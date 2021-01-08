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
    subDir.mkdirs
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
    resultList.par.map(rb => Util.writePng(rb.annotatedImage, annotatedImageFile(subDir, rb.symmetryAndFlatness.beamName)))


    val dynamicContent = {
      <div class="col-md-6 col-md-offset-3">
        <embed type="text/html" src={ "/admin/SymmetryAndFlatnessSubHTML?outputPK=" + extendedData.output.outputPK.get }/>
      </div>
    }
    val html = Phase2Util.wrapSubProcedure(extendedData, dynamicContent, title = "Symmetry and Flatness", status, None, runReq)
    /*  TODO
    Util.writeFile(mainHtmlFile, html)

    resultList.map(rb => SymmetryAndFlatnessBeamProfileHTML.makeDisplay(subDir, extendedData, rb, status, runReq))

    summary(mainHtmlFile, resultList.size, status)
     */
    <div>TODO</div>
  }

}
