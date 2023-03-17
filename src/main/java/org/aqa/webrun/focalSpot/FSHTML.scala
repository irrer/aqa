package org.aqa.webrun.focalSpot

import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.Util

import java.io.File
import scala.xml.Elem

object FSHTML {

  /**
    * Make the HTML for the given data set.
    * @param extendedData metadata.
    * @param fsSet Result of focal spot analyses.
    * @return Summary for main Phase3 page.
    */
  def makeHtml(extendedData: ExtendedData, runReq: RunReq, fsSet: Seq[FSSet]): Elem = {
    val htmlFileName = "FocalSpot.html"

    val content = <div>Hey</div>
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, None, runReq)
    val file = new File(extendedData.output.dir, htmlFileName)
    Util.writeBinaryFile(file, text.getBytes)

    <div>Not much of a summary.</div>
  }

}
