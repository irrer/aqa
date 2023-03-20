package org.aqa.webrun.focalSpot

import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.Util

import java.io.File
import scala.xml.Elem

object FSsubHTML {

  /**
    * Make the main HTML content for the given data set.
    *
    * @param extendedData metadata.
    * @param fsSet        Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  private def makeContent(extendedData: ExtendedData, runReq: RunReq, fsSet: FSSet): Elem = {
    <div class="row">
      <div class="row">
        <div class="col-md-2 col-md-offset-5">
          <table class="table table-responsive table-bordered" style="margin-top:25px;">
            <tr>
              <th>MV</th>
              <th>X Alignment (mm)</th>
              <th>Y Alignment (mm)</th>
            </tr>
            {fsSet}
          </table>
        </div>
      </div>
      <div class="row">
        Put main chart here
      </div>
    </div>
  }

  /**
    * Make the HTML for the given data set.
    * @param extendedData metadata.
    * @param fsSet Result of focal spot analyses.
    * @return URL for this HTML
    */
  def makeHtml(extendedData: ExtendedData, runReq: RunReq, fsSet: FSSet): Unit = {
    val mainHtmlFile = {
      val dir = FSHTML.focalSpotDir(extendedData.output)
      new File(dir, fsSet.htmlFileName)
    }

    val content = makeContent(extendedData, runReq, fsSet: FSSet)
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, None, runReq)
    Util.writeBinaryFile(mainHtmlFile, text.getBytes)
  }

}
