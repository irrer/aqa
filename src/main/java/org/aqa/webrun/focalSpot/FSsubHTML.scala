package org.aqa.webrun.focalSpot

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import org.aqa.Logging
import org.aqa.web.WebUtil

import java.io.File
import java.text.SimpleDateFormat
import scala.xml.Elem

object FSsubHTML extends Logging {

  /* Name of file where Matlab code is written. */
  private def matlabFileName(fsSet: FSSet) = s"matlabMV${fsSet.mvText}.txt"

  private def fmtTd(d: Double): Elem = {
    <td title={FSHTML.fmtHi(d)}>{FSHTML.fmtLo(d)}</td>
  }

  private def row(fsMeasure: FSMeasure): Elem = {
    val fs = fsMeasure.focalSpot

    <tr>
      <td>{if (fsMeasure.isJaw) "Jaw" else "MLC"}</td>
      <td>{fsMeasure.collimatorAngleRounded_deg}</td>
      <td style="white-space: nowrap;">{Util.normalizeBeamName(fsMeasure.beamName)}</td>
      {fmtTd(fs.centerX)}
      {fmtTd(fs.centerY)}
      {fmtTd(fs.topEdge_mm - fs.topEdgePlanned_mm)}
      {fmtTd(fs.bottomEdge_mm - fs.bottomEdgePlanned_mm)}
      {fmtTd(fs.leftEdge_mm - fs.leftEdgePlanned_mm)}
      {fmtTd(fs.rightEdge_mm - fs.rightEdgePlanned_mm)}
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
      val dir = FSHTML.focalSpotDir(extendedData)
      val typeName = if (fsMeasure.isJaw) "Jaw" else "MLC"
      val nomEnergy = fsMeasure.NominalBeamEnergy.round
      val imageId = fsMeasure.fileNamePrefix
      val pngFileName = fsMeasure.fileNamePrefix + ".png"
      val pngFile = new File(dir, pngFileName)
      Util.writePng(fsMeasure.bufferedImage, pngFile)

      val title = nomEnergy + "MV " + typeName + " " + fsMeasure.collimatorAngleRounded_deg

      val htmlFileName = fsMeasure.fileNamePrefix + ".html"
      val htmlFile = new File(dir, htmlFileName)

      val contentTimeText = {
        val dateTime = DicomUtil.getTimeAndDate(fsMeasure.rtimage, TagByName.ContentDate, TagByName.ContentTime).get
        val fmt = new SimpleDateFormat("H:mm:ss")
        Util.formatDate(fmt, dateTime)
      }

      val ImageHashText = {
        val dateTime = DicomUtil.getTimeAndDate(fsMeasure.rtimage, TagByName.ContentDate, TagByName.ContentTime).get

        val fmt = new SimpleDateFormat("H:mm:ss")
        Util.formatDate(fmt, dateTime)
      }

      val viewDicom = {
        <div class="col-md-10 col-md-offset-1">
          <center><h3>{title}</h3></center>
          <center>{contentTimeText}</center>
          <img src={pngFileName}/>
          <p style="margin-top:20px;"> </p>
          <pre>
            {WebUtil.nl + DicomUtil.attributeListToString(fsMeasure.rtimage)}
          </pre>
          <p style="margin-top:80px;"> </p>
        </div>
      }

      val htmlText = WebUtil.wrapBody(viewDicom, title)
      Util.writeFile(htmlFile, htmlText)

      <td style="padding:12px;">
        <a href={htmlFileName}>
          <center><h3>{title}</h3></center>
          <center>{contentTimeText}</center>
          <div class="zoom" id={fsMeasure.fileNamePrefix}>
            <img width="500" src={pngFileName}/>
          </div>
        </a>
      </td>
    }

    /** Table of center values. */
    def centerTable: Elem = {

      <table class="table table-responsive table-bordered" style="margin-top:25px;">
        <tr>
          <th>Type</th>
          <th>Center X</th>
          <th>Center Y</th>
        </tr>
        <tr>
          <td>Jaw</td>
          {fmtTd(fsSet.jawXCenter)}
          {fmtTd(fsSet.jawYCenter)}
        </tr>
        <tr>
          <td>MLC</td>
          {fmtTd(fsSet.mlcXCenter)}
          {fmtTd(fsSet.mlcYCenter)}
        </tr>
      </table>
    }

    def beamTable: Elem = {
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
    }

    def aTable: Elem = {
      <table class="table table-responsive table-bordered" style="margin-top:25px;">
        <tr>
          <th>aX</th>
          <th>aY</th>
        </tr>
        <tr>
          {fmtTd(fsSet.aX)}
          {fmtTd(fsSet.aY)}
        </tr>
      </table>
    }

    // write Matlab file
    val matlabFile = new File(FSHTML.focalSpotDir(extendedData), matlabFileName(fsSet))
    val matlabText = FSMatlab.generateMatlabCode(fsSet)
    Util.writeFile(matlabFile, matlabText)

    val fluenceText = if (fsSet.jaw090.isFFF) "FFF " else ""

    <div class="row">
      <center>
        <h2>{mvText} {fluenceText}MV Alignment: {alignment}</h2>
        <a href={FSHTML.htmlFileName} title="Return to focal spot summary page.">Focal Spot Summary</a>
        <a href={matlabFileName(fsSet)} title="Download Matlab code demonstrating calculations." style="margin-left:50px;">Matlab</a>
      </center>
      <div class="row">
        <div class="col-md-4 col-md-offset-4">
          {centerTable}
        </div>
      </div>
      <div class="row">
        <div class="col-md-2 col-md-offset-5">
          {aTable}
        </div>
      </div>
      <div class="row">
        <div class="col-md-8 col-md-offset-2">
          {beamTable}
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
       |<script src="/FSHistoryRestlet?${FSHistoryRestlet.outputPKTag}=${extendedData.output.outputPK.get}&${FSHistoryRestlet.mvTag}=${fsSet.jaw090.NominalBeamEnergy}&${FSHistoryRestlet.fluenceNameTag}=${fsSet.jaw090.fluenceName}"></script>
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
      val dir = FSHTML.focalSpotDir(extendedData)
      new File(dir, fsSet.htmlFileName)
    }

    val fsMvChart = new FSmvChart(extendedData.output.outputPK.get, fsSet.jaw090.focalSpot.KVP_kv / 1000, fsSet.jaw090.fluenceName)

    val content = makeContent(extendedData, fsSet: FSSet, fsMvChart)

    val javascript = zoomScript + chartScript(extendedData, fsSet)
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, runScript = Some(javascript), fsRunReq.rtimageMap)
    Util.writeBinaryFile(mainHtmlFile, text.getBytes)
  }

}
