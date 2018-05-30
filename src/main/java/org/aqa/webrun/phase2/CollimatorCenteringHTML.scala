package org.aqa.webrun.phase2

import org.aqa.db.Output
import scala.xml.Elem
import java.awt.image.BufferedImage
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import javax.imageio.ImageIO
import org.aqa.run.ProcedureStatus
import org.aqa.Util

object CollimatorCenteringHTML {

  private val htmlFileName = "CollimatorCentering.html"

  private def showImage(fileName: String, outputDir: File, bufImg: BufferedImage): Elem = {
    val fn = FileUtil.replaceInvalidFileNameCharacters(fileName, '_')
    val pngFile = new File(outputDir, fn)
    ImageIO.write(bufImg, "png", pngFile)
    <div>
      <center>
        <a href={ fn }>
          <img src={ fn } align="middle" width="400"/>
        </a>
      </center>
    </div>
  }

  /**
   * Make a tiny summary and link to the detailed report.
   */
  def makeSummary(status: ProcedureStatus.Value) = {
    val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={ htmlFileName }>
          Collimator Centering<br/>
          <img src={ iconImage } height="32"/>
        </a>
      </div>
    }
    elem
  }

  def makeDisplay(extendedData: ExtendedData, status: ProcedureStatus.Value, image090: MeasureNSEWEdges.AnalysisResult, image270: MeasureNSEWEdges.AnalysisResult, runReq: RunReq): Elem = {

    val outputDir = extendedData.output.dir

    val content = {
      <div>
        <div class="col-md-5" align="middle">
          <h3 title="Gantry Angle 90" style="text-align:center;">90</h3>
          { showImage("CollimatorCentering090_" + Config.CollimatorCentering090BeamName + ".png", outputDir, image090.bufferedImage) }
        </div>
        <div class="col-md-5 col-md-offset-1" align="middle">
          <h3 title="Gantry Angle 270" style="text-align:center;">270</h3>
          { showImage("CollimatorCentering270_" + Config.CollimatorCentering270BeamName + ".png", outputDir, image270.bufferedImage) }
        </div>
      </div>
    }

    val html = Phase2Util.wrapSubProcedure(extendedData, content, "Collimator Centering", status)
    val outFile = new File(outputDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status)
  }

}