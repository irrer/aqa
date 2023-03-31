package org.aqa.webrun.focalSpot

import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import org.aqa.Logging

import java.io.File
import scala.xml.Elem

object FSsubHTML extends Logging {

  /* Name of file where Matlab code is written. */
  private def matlabFileName(fsSet: FSSet) = s"matlabMV${fsSet.mvText}.txt"

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
  private def makeContent(extendedData: ExtendedData, fsSet: FSSet, fsMvChart: FSmvChart): Elem = {

    val mvText = fsSet.mvText

    val alignment: Elem = {
      val x = fsSet.focalSpotAlignmentX_mm
      val y = fsSet.focalSpotAlignmentY_mm
      <span title={FSHTML.fmtHi(x) + ", " + FSHTML.fmtHi(y)}>
      {FSHTML.fmtLo(x) + ", " + FSHTML.fmtLo(y)}
      </span>
    }

    def showImage(fsMeasure: FSMeasure): Elem = {
      val dir = FSHTML.focalSpotDir(extendedData.output)
      val typeName = if (fsMeasure.isJaw) "Jaw" else "MLC"
      val imageId = typeName + fsMeasure.collimatorAngleRounded_deg.formatted("%03d")
      val pngFileName = imageId + ".png"
      val pngFile = new File(dir, pngFileName)
      Util.writePng(fsMeasure.bufferedImage, pngFile)
      <td style="padding:12px;">
        <a href={pngFileName}>
          <center><h3>{typeName + " " + fsMeasure.collimatorAngleRounded_deg}</h3></center>
          <div class="zoom" id={imageId}>
            <img width="600" src={pngFileName}/>
          </div>
        </a>
      </td>
    }

    // write Matlab file
    val matlabFile = new File(FSHTML.focalSpotDir(extendedData.output), matlabFileName(fsSet))
    val matlabText = FSMatlab.generateMatlabCode(fsSet)
    Util.writeFile(matlabFile, matlabText)

    <div class="row">
      <center>
        <h2>MV: {mvText} Alignment: {alignment}</h2>
        <a href={FSHTML.htmlFileName} title="Return to focal spot summary page.">Focal Spot Summary</a>
        <a href={matlabFileName(fsSet)} title="Download Matlab code demonstrating calculations." style="margin-left:50px;">Matlab</a>
      </center>
      <div class="row">
        <div class="col-md-8 col-md-offset-2">
          <table class="table table-responsive table-bordered" style="margin-top:25px;">
            <tr>
              <th>Type</th>
              <th>Collimator Angle mm</th>
              <th>Beam Name</th>
              <th>X Center mm</th>
              <th>Y Center mm</th>
              <th>Top Edge Error mm</th>
              <th>Bottom Edge Error mm</th>
              <th>Left Edge Error mm</th>
              <th>Right Edge Error mm</th>
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
          {C3ChartHistory.htmlHelp()}
          <h3>Focal Spot and Centers</h3>
          {fsMvChart.chart._1.html}
          <h3>Edge Errors</h3>
          {fsMvChart.chart._2.html}
        </div>
      </div>
      <div class="row">
        <div class="col-md-10 col-md-offset-2">
          <table>
            <tr>
              {showImage(fsSet.jaw090)}
              {showImage(fsSet.jaw270)}
            </tr>
            <tr>
              {showImage(fsSet.mlc090)}
              {showImage(fsSet.mlc270)}
            </tr>
          </table>
        </div>
      </div>
    </div>
  }

  /**
    * Enable zoom for the images.
    */
  private val zoomScript: String = {
    """
      |<script>
      |  $(document).ready(function(){ $('#Jaw090').zoom(); });
      |  $(document).ready(function(){ $('#Jaw270').zoom(); });
      |  $(document).ready(function(){ $('#MLC090').zoom(); });
      |  $(document).ready(function(){ $('#MLC270').zoom(); });
      |</script>
      |
      |""".stripMargin
  }

  private def chartScript(extendedData: ExtendedData, fsSet: FSSet): String = {
    s"""
       |
       |<script src="/FSHistoryRestlet?${FSHistoryRestlet.outputPKTag}=${extendedData.output.outputPK.get}&${FSHistoryRestlet.mvTag}=${fsSet.jaw090.NominalBeamEnergy}"></script>
       |
       |""".stripMargin
  }

  /**
    * Make the HTML for the given data set.
    * @param extendedData metadata.
    * @param fsSet Result of focal spot analyses.
    * @return URL for this HTML
    */
  def makeHtml(extendedData: ExtendedData, fsRunReq: FSRunReq, fsSet: FSSet): Unit = {
    val mainHtmlFile = {
      val dir = FSHTML.focalSpotDir(extendedData.output)
      new File(dir, fsSet.htmlFileName)
    }

    val fsMvChart = new FSmvChart(extendedData.output.outputPK.get, fsSet.jaw090.focalSpot.KVP_kv / 1000)

    val content = makeContent(extendedData, fsSet: FSSet, fsMvChart)

    val javascript = zoomScript + chartScript(extendedData, fsSet) + fsMvChart.chartPair._1.javascript + fsMvChart.chartPair._2.javascript
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, runScript = Some(javascript), fsRunReq.rtimageMap)
    Util.writeBinaryFile(mainHtmlFile, text.getBytes)
  }

}
