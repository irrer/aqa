package org.aqa.webrun.focalSpot

import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import org.aqa.web.WebServer
import org.aqa.Config
import org.aqa.db.Output
import org.aqa.web.C3ChartHistory

import java.io.File
import scala.xml.Elem

object FSHTML {

  val focalSpotDirName = "FocalSpot"
  val htmlFileName = "FocalSpot.html"

  def focalSpotDir(output: Output): File = new File(output.dir, focalSpotDirName)

  def fmtLo(d: Double): String = {
    if (d.round == d)
      d.round.toString
    else
      d.formatted("%10.2f").trim
  }

  def fmtHi(d: Double): String = {
    if (d.round == d)
      d.round.toString
    else
      d.formatted("%10.6f").trim
  }

  private def summary(mainHtmlFile: File, fsSetList: Seq[FSSet]) = {
    val iconImage = Config.passImageUrl

    def formatFsSet(fsSet: FSSet): Elem = {

      val title = {
        "X: " + fmtHi(fsSet.focalSpotAlignmentX_mm) + ", " + fmtHi(fsSet.focalSpotAlignmentY_mm)
      }

      <div title={title}>{fsSet.mvText} MV : {fmtLo(fsSet.focalSpotAlignmentX_mm) + ", " + fmtLo(fsSet.focalSpotAlignmentY_mm)}</div>
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
      {fmt(fsSet.focalSpotAlignmentX_mm)}
      {fmt(fsSet.focalSpotAlignmentY_mm)}
      <td><a href={s"${fsSet.htmlFileName}"}>Details</a></td>
    </tr>
  }

  /**
    * Make summary table.
    * @param fsSetList    Result of focal spot analyses.
    * @return table HTML.
    */
  private def htmlTable(fsSetList: Seq[FSSet]): Elem = {
    <table class="table table-responsive table-bordered" style="margin-top:25px;">
      <tr>
        <th>MV</th>
        <th>X Alignment (mm)</th>
        <th>Y Alignment (mm)</th>
        <th>Details</th>
      </tr>{fsSetList.map(fsRow)}
    </table>
  }

  /**
    * Make the main HTML content for the given data set.
    *
    * @param extendedData metadata.
    * @param fsRunReq DICOM data.
    * @param fsSetList    Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  private def makeContent(extendedData: ExtendedData, fsRunReq: FSRunReq, fsSetList: Seq[FSSet], chartHtml: Elem): Elem = {
    <div class="row">
      <div class="row">
        <div class="col-md-2 col-md-offset-5">
          {htmlTable(fsSetList)}
        </div>
      </div>
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          {C3ChartHistory.htmlHelp()}
          {chartHtml}
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
  def makeHtml(extendedData: ExtendedData, fsRunReq: FSRunReq, fsSetList: Seq[FSSet]): Elem = {
    val dir = focalSpotDir(extendedData.output)
    dir.mkdirs()

    val mainChart = new FSMainChart(outputPK = extendedData.output.outputPK.get).chart

    fsSetList.foreach(fsSet => FSsubHTML.makeHtml(extendedData, fsRunReq, fsSet))

    val content = makeContent(extendedData, fsRunReq, fsSetList.sortBy(_.jaw090.NominalBeamEnergy), mainChart.html)
    val javascript = s"""<script src="${FSHistoryRestlet.path}?${FSHistoryRestlet.outputPKTag}=${extendedData.output.outputPK.get}"></script>"""
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, runScript = Some(javascript), rtimageMap = fsRunReq.rtimageMap)
    val mainHtmlFile = new File(dir, htmlFileName)
    Util.writeBinaryFile(mainHtmlFile, text.getBytes)

    summary(mainHtmlFile, fsSetList)
  }

}
