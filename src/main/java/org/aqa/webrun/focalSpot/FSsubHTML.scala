package org.aqa.webrun.focalSpot

import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.Util

import java.io.File
import scala.xml.Elem

object FSsubHTML {

  private def row(fsMeasure: FSMeasure): Elem = {
    val fs = fsMeasure.focalSpot

    def fmt(d: Double): Elem = {
      <td title={FSHTML.fmtHi(d)}>{FSHTML.fmtLo(d)}</td>
    }

    <tr>
      <td>{if (fsMeasure.isJaw) "Jaw" else "MLC"}</td>
      <td>{fsMeasure.collimatorAngleRounded_deg}</td>
      <td style="white-space: nowrap;">{fsMeasure.beamName}</td>
      {fmt(fs.centerX)}
      {fmt(fs.centerY)}
      {fmt(fs.topEdge_mm - fs.topEdgePlanned_mm)}
      {fmt(fs.bottomEdge_mm - fs.bottomEdgePlanned_mm)}
      {fmt(fs.leftEdge_mm - fs.leftEdgePlanned_mm)}
      {fmt(fs.rightEdge_mm - fs.rightEdgePlanned_mm)}
    </tr>
  }

  /**
    * Make the main HTML content for the given data set.
    *
    * @param extendedData metadata.
    * @param fsSet        Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  private def makeContent(extendedData: ExtendedData, runReq: RunReq, fsSet: FSSet): Elem = {
    val mvText = {
      val mv = fsSet.jaw090.NominalBeamEnergy / 1000.0
      if (mv.round == mv)
        mv.round.toString
      else
        mv.toString
    }

    val alignment: Elem = {
      val x = fsSet.focalSpotAlignmentX_mm
      val y = fsSet.focalSpotAlignmentY_mm
      <span title={FSHTML.fmtHi(x) + ", " + FSHTML.fmtHi(y)}>
      {FSHTML.fmtLo(x) + ", " + FSHTML.fmtLo(y)}
      </span>
    }

    <div class="row">
      <center>
        <h2>MV: {mvText} Alignment: {alignment}</h2>
      </center>
      <div class="row">
        <div class="col-md-8 col-md-offset-2">
          <table class="table table-responsive table-bordered" style="margin-top:25px;">
            <tr>
              <th>Type</th>
              <th>Collimator Angle</th>
              <th>Beam Name</th>
              <th>X Center</th>
              <th>Y Center</th>
              <th>Top Edge Error</th>
              <th>Bottom Edge Error</th>
              <th>Left Edge Error</th>
              <th>Right Edge Error</th>
            </tr>
            {row(fsSet.jaw090)}
            {row(fsSet.jaw270)}
            {row(fsSet.mlc090)}
            {row(fsSet.mlc270)}
          </table>
        </div>
      </div>
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          Put sub chart here
        </div>
      </div>
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          Put images here
        </div>
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
