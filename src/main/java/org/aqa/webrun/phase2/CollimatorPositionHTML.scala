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
import org.aqa.db.CollimatorCentering
import java.awt.geom.Point2D

object CollimatorCenteringHTML {

  private val htmlFileName = "CollimatorCentering.html"

  private def fmt(d: Double): String = d.formatted("%8.2f")

  private def showImage(fileName: String, outputDir: File, bufImg: BufferedImage): Elem = {
    val fn = FileUtil.replaceInvalidFileNameCharacters(fileName, '_')
    val pngFile = new File(outputDir, fn)
    Util.writePng(bufImg, pngFile)
    <div>
      <center>
        <a href={ fn }>
          <img src={ fn } align="middle" width="400"/>
        </a>
      </center>
    </div>
  }

  private def makeTable(collimatorCentering: CollimatorCentering): Elem = {
    val cc = collimatorCentering
    case class CCRow(title: String, value: Double)

    val rowList = Seq(
      new CCRow("X collimator center - image center", cc.xCollimatorCenterMinusImageCenter_mm),
      new CCRow("Y collimator center - image center", cc.yCollimatorCenterMinusImageCenter_mm),
      new CCRow("X collimator center", cc.xCollimatorCenter_mm),
      new CCRow("Y collimator center", cc.yCollimatorCenter_mm),
      new CCRow("north edge 90 degrees", cc.north090_mm),
      new CCRow("south edge 90 degrees", cc.south090_mm),
      new CCRow("east edge 90 degrees", cc.east090_mm),
      new CCRow("west edge 90 degrees", cc.west090_mm),
      new CCRow("north edge 270 degrees", cc.north270_mm),
      new CCRow("south edge 270 degrees", cc.south270_mm),
      new CCRow("east edge 270 degrees", cc.east270_mm),
      new CCRow("west edge 270 degrees", cc.west270_mm: Double))

    def rowToHtml(ccRow: CCRow): Elem = {
      <tr>
        <td>{ ccRow.title }</td>
        <td>{ fmt(ccRow.value) }</td>
      </tr>
    }

    val table = {
      <table class="table table-striped" title="All values in mm">
        <tr>
          <th>Description</th>
          <th>Value</th>
        </tr>
        { rowList.map(row => rowToHtml(row)) }
      </table>
    }
    table
  }

  /**
   * Make a tiny summary and link to the detailed report.
   */
  private def makeSummary(status: ProcedureStatus.Value) = {
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

  def makeDisplay(extendedData: ExtendedData, collimatorCentering: CollimatorCentering, status: ProcedureStatus.Value, image090: MeasureNSEWEdges.AnalysisResult, image270: MeasureNSEWEdges.AnalysisResult, runReq: RunReq): Elem = {

    val outputDir = extendedData.output.dir

    val imageCenter = new Point2D.Double(runReq.floodOriginalImage.width * runReq.ImagePlanePixelSpacing.getX / 2, runReq.floodOriginalImage.height * runReq.ImagePlanePixelSpacing.getY / 2)

    def imageTitle(name: String, ar: MeasureNSEWEdges.AnalysisResult): Elem = {
      val err = fmt(ar.measurementSet.center.getX - imageCenter.getX) + ", " + fmt(ar.measurementSet.center.getY - imageCenter.getY)
      <h3 title="Gantry Angle and center minus image center." style="text-align:center;">{ name + " : " + err }</h3>
    }

    val content = {
      <div>
        <div class="col-md-4 col-md-offset-4" align="middle">
          { makeTable(collimatorCentering) }
        </div>
        <div class="row" title="Click images for full sized view">
          <div class="col-md-5" align="middle">
            { imageTitle("90", image090) }
            { showImage("CollimatorCentering090_" + Config.CollimatorCentering090BeamName + ".png", outputDir, image090.bufferedImage) }
          </div>
          <div class="col-md-5 col-md-offset-1" align="middle">
            { imageTitle("270", image270) }
            { showImage("CollimatorCentering270_" + Config.CollimatorCentering270BeamName + ".png", outputDir, image270.bufferedImage) }
          </div>
        </div>
      </div>
    }

    val html = Phase2Util.wrapSubProcedure(extendedData, content, "Collimator Centering", status, None)
    val outFile = new File(outputDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status)
  }

}