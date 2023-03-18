package org.aqa.webrun.focalSpot

import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.Util
import org.aqa.web.WebServer
import org.aqa.Config

import java.io.File
import scala.xml.Elem

object FSHTML {

  def fmtLo(d: Double): String = {
    if (d.round == d)
      d.round.toString
    else
      d.formatted("%10.2f").trim
  }

  def fmtHi(d: Double) = {
  if (d.round == d)
      d.round.toString
    else
      d.formatted("%10.6f").trim
  }

  private def summary(mainHtmlFile: File, fsSetList: Seq[FSSet]) = {
    val iconImage = Config.passImageUrl

    def formatFsSet(fsSet: FSSet): Elem = {

      val title = {
        "X: " + fmtHi(fsSet.focalSpotAlignmentX) + ", " + fmtHi(fsSet.focalSpotAlignmentY)
      }

      <div title={title}>{fmtLo(fsSet.jaw090.NominalBeamEnergy)} MV : {fmtLo(fsSet.focalSpotAlignmentX) + ", " + fmtLo(fsSet.focalSpotAlignmentY)}</div>
    }

    val elem = {
      <div>
        <a href={WebServer.urlOfResultsFile(mainHtmlFile)}>
          Focal Spot
          {fsSetList.map(formatFsSet)}
          <img src={iconImage} height="32"/>
        </a>
      </div>
    }
    elem
  }

  private def fsRow(fsSet: FSSet): Elem = {
    def fmt(d: Double) = {
      <td title={fmtHi(d)}>{fmtLo(d)}</td>
    }

    <tr>
      {fmt(fsSet.jaw090.NominalBeamEnergy)}
      {fmt(fsSet.focalSpotAlignmentX)}
      {fmt(fsSet.focalSpotAlignmentY)}
    </tr>
  }

  /**
    * Make the main HTML content for the given data set.
    *
    * @param extendedData metadata.
    * @param fsSetList    Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  private def makeContent(extendedData: ExtendedData, runReq: RunReq, fsSetList: Seq[FSSet]): Elem = {
    <div class="row">
      <div class="row">
        <div class="col-md-2 col-md-offset-5">
          <table class="table table-responsive table-bordered" style="margin-top:25px;">
            <tr>
              <th>MV</th>
              <th>X Alignment (mm)</th>
              <th>Y Alignment (mm)</th>
            </tr>
            {fsSetList.map(fsRow)}
          </table>
        </div>
      </div>
    </div>
  }

  /**
    * Make the HTML for the given data set.
    * @param extendedData metadata.
    * @param fsSetList Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  def makeHtml(extendedData: ExtendedData, runReq: RunReq, fsSetList: Seq[FSSet]): Elem = {
    val htmlFileName = "FocalSpot.html"
    val focalSpotDir = new File(extendedData.output.dir, "FocalSpot")
    focalSpotDir.mkdirs()

    val content = makeContent(extendedData, runReq, fsSetList.sortBy(_.jaw090.NominalBeamEnergy))
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, None, runReq)
    val mainHtmlFile = new File(focalSpotDir, htmlFileName)
    Util.writeBinaryFile(mainHtmlFile, text.getBytes)

    summary(mainHtmlFile, fsSetList)
  }

}
