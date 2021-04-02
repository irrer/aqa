package org.aqa.webrun.mlcqa

import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

import java.io.File
import scala.xml.Elem

object MlcQaHtml {

  def makeDisplay(extendedData: ExtendedData, runReq: MlcQaRunReq, fleList: Seq[FindLeafEnds], procedureStatus: ProcedureStatus.Value): Unit = {

    def beamNameOf(fle: FindLeafEnds): String = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, fle.image).get

    def imageFileOf(fle: FindLeafEnds): File = {
      val pngFileName = FileUtil.replaceInvalidFileNameCharacters(beamNameOf(fle), '_').replace(' ', '_') + ".png"
      new File(extendedData.output.dir, pngFileName)
    }

    def fleToHtml(fle: FindLeafEnds): Elem = {

      val beamName = beamNameOf(fle)
      val pngFile = imageFileOf(fle)
      Util.writePng(fle.bufferedImage, pngFile)

      def td(d: Double): Elem = {
        <td title={d.toString}>{d.formatted("%8.2f").trim}</td>
      }

      val skewTable = {
        // @formatter:off
        <table class="table table-bordered">
          <thead>
            <tr>
              <th>
                Position (mm)
              </th>
              <th>
                Left - Right (mm)
              </th>
              <th>
                Left (mm)
              </th>
              <th>
                Right (mm)
              </th>
              <th>
                Planned (mm)
              </th>
            </tr>

            <tr>
              <td>
                Top
              </td>{td(fle.leafSet.topLeft - fle.leafSet.topRight)}{td(fle.leafSet.topLeft)}{td(fle.leafSet.topRight)}{td(fle.leafPositionRtplanTop_mm)}
            </tr>

            <tr>
              <td>
                Bottom
              </td>{td(fle.leafSet.bottomLeft - fle.leafSet.bottomRight)}{td(fle.leafSet.bottomLeft)}{td(fle.leafSet.bottomRight)}{td(fle.leafPositionRtplanBottom_mm)}
            </tr>

          </thead>
        </table>
      // @formatter:on
      }

      val offsetTable = {
        // @formatter:off
          <table class="table table-bordered">
          <thead>
            <tr>
              <th>
                Position (mm)
              </th>
              <th>
                Top - Bottom - <br></br> Planned (mm)
              </th>
              <th>
                Top (mm)
              </th>
              <th>
                Bottom (mm)
              </th>
            </tr>

            <tr>
              <td>
                Left
              </td>
                {td(fle.leafSet.topLeft - fle.leafSet.bottomLeft - (fle.leafPositionRtplanTop_mm - fle.leafPositionRtplanBottom_mm))}
                {td(fle.leafSet.topLeft)}
                {td(fle.leafSet.topRight)}
            </tr>
          
            <tr>
              <td>
                Right
              </td>
              {td(fle.leafSet.bottomRight - fle.leafSet.topRight + (fle.leafPositionRtplanTop_mm - fle.leafPositionRtplanBottom_mm))}
              {td(fle.leafSet.bottomLeft)}
              {td(fle.leafSet.bottomRight)}
            </tr>
          
          </thead>
        </table>
      // @formatter:on
      }

      val content = {
        <div class="row">
            <hr/>
            <div class="col-md-6">
              <center>
              <h3 style="margin:20px;">
                {beamName}
              </h3>
                {skewTable}
                <p></p>
                {offsetTable}
              </center>
            </div>
            <div class="col-md-6">
              <center>
              <a href={pngFile.getName} style="margin:8px;">
                Click image for larger view.
                <div class="zoom" id={pngFile.getName.dropRight(4)}>
                  <img height="400;" src={pngFile.getName}/>
                </div>
              </a>
              </center>
            </div>
        </div>
      }

      content
    }

    val script = {
      def toZoom(fle: FindLeafEnds): String = {
        "$(document).ready(function(){ $('#" + imageFileOf(fle).getName.dropRight(4) + "').zoom(); });"
      }
      "\n<script>\n" +
        fleList.map(toZoom).mkString("\n\n") +
        "\n</script>\n"
    }

    val content: Elem = {
      <div class="row">
        <div class="col-md-8 col-md-offset-2">
          {fleList.sortBy(beamNameOf).map(fleToHtml)}
        </div>
      </div>
    }

    val contentWithHeader = ExtendedData.wrapExtendedData(extendedData, content)
    val text = WebUtil.wrapBody(contentWithHeader, "MLC QA", refresh = None, c3 = true, runScript = Some(script))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)

  }

}
