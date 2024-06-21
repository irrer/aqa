package org.aqa.webrun.focalSpot

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.RawByte
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import org.aqa.web.WebServer
import org.aqa.Config
import org.aqa.db.Output
import org.aqa.web.C3ChartHistory
import org.aqa.web.WebUtil
import org.aqa.Logging

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

object FSHTML extends Logging {

  private val focalSpotDirName = "FocalSpot"
  val htmlFileName: String = Output.displayFilePrefix + ".html"

  def focalSpotDir(extendedData: ExtendedData): File = {
    if (extendedData.procedure.isFocalSpot)
      extendedData.output.dir
    else
      new File(extendedData.output.dir, focalSpotDirName)
  }

  def fmtLo(d: Double): String = {
    if (d.round == d)
      d.round.toString
    else
      d.formatted("%10.3f").trim
  }

  def fmtHi(d: Double): String = {
    if (d.round == d)
      d.round.toString
    else
      d.formatted("%10.6f").trim
  }

  private def summary(mainHtmlFile: File, fsSetLis1: Seq[FSSet]) = {
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
          { /*  fsSetLis1.map(formatFsSet)  TODO put back */ }
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

    val id = fsSet.jaw090.mvText + (if (fsSet.jaw090.isFFF) "-FFF" else "")

    <tr>
      <td>{id}</td>
      {fmt(fsSet.focalSpotAlignmentX_mm)}
      {fmt(fsSet.focalSpotAlignmentY_mm)}
      <td><a href={s"${fsSet.htmlFileName}"}>Details</a></td>
    </tr>
  }

  /**
    * Make summary table.
    * @param fsSetLis2    Result of focal spot analyses.
    * @return table HTML.
    */
  private def htmlTable(fsSetLis2: Seq[FSSet]): Elem = {
    <table class="table table-responsive table-bordered" style="margin-top:25px;">
      <tr>
        <th>MV</th>
        <th>X Alignment (mm)</th>
        <th>Y Alignment (mm)</th>
        <th>Details</th>
      </tr>{fsSetLis2.map(fsRow)}
    </table>
  }

  private def rtplanHtml(extendedData: ExtendedData, fsRunReq: FSRunReq, fsSetLis3: Seq[FSSet]): Elem = {
    val fmt = new SimpleDateFormat("H:mm:ss")

    def getContentDateTime(rtimage: AttributeList): Date = {
      DicomUtil.getTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get
    }

    val measureList = fsSetLis3.flatMap(s => s.measureList).sortBy(m => getContentDateTime(m.rtimage).getTime)

    measureList.head.rtimage

    val beamTable = {

      def measureToRow(fsm: FSMeasure): Elem = {

        val dateTimeText = Util.formatDate(fmt, getContentDateTime(fsm.rtimage))

        val hash = {
          val text = RawByte.formatByteArray(edu.umro.ScalaUtil.Crypto.hash(DicomUtil.PixelDataToByteArray(fsm.rtimage)))
          text
        }

        <tr>
          <td>{<a href={fsm.fileNamePrefix + ".html"}>{Util.normalizeBeamName(fsm.beamName)}</a>}</td>
          <td>{dateTimeText}</td>
          <td>{fsm.beamNumber}</td>
          <td>{hash}</td>
        </tr>
      }

      <table class="table table-bordered" style="width=400px;">
        <thead>
          <tr>
            <th>Beam Name</th>
            <th>Content Time</th>
            <th>Beam Number</th>
            <th>Image Hash</th>
          </tr>
        </thead>
        {measureList.map(measureToRow)}
      </table>
    }

    val planContent = {
      <div class="row">
        <div class="col-md-6 col-md-offset-3">
          <center>
            <h3>RTPLAN</h3>
          </center>
          <p style="margin-top:20px;"> </p>
          <a href="display.html">Main Report</a>
          <p style="margin-top:20px;"> </p>
          <center>
            <h4>Beam Table</h4>
            Ordered by Content Time
            {beamTable}
          </center>
        </div>
        <div class="col-md-10 col-md-offset-1">
          <p style="margin-top:20px;"> </p>
          <pre>{WebUtil.nl + DicomUtil.attributeListToString(fsRunReq.rtplan)}</pre>
          <p style="margin-top:100px;"> </p>
        </div>
      </div>
    }

    val htmlText = WebUtil.wrapBody(planContent, "RTPLAN")
    val rtplanFile = new File(extendedData.output.dir, "rtplan.html")
    Util.writeFile(rtplanFile, htmlText)

    <center>
      <a href={rtplanFile.getName}>View RTPLAN</a>
    </center>
  }

  /**
    * Make the main HTML content for the given data set.
    *
    * @param extendedData metadata.
    * @param fsRunReq DICOM data.
    * @param fsSetLis4    Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  private def makeContent(extendedData: ExtendedData, fsRunReq: FSRunReq, fsSetLis4: Seq[FSSet], chartHtml: Elem): Elem = {
    <div class="row">
      <div class="row">
        <div class="col-md-3 col-md-offset-4">
          {htmlTable(fsSetLis4)}
          <p>{rtplanHtml(extendedData, fsRunReq, fsSetLis4)}</p>
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
    * @param fsSetLis5 Result of focal spot analyses.
    * @return Report for all focal spot sets.
    */
  def makeHtml(extendedData: ExtendedData, fsRunReq: FSRunReq, fsSetLis5: Seq[FSSet]): Elem = {
    val dir = focalSpotDir(extendedData)
    dir.mkdirs()

    val mainChart = new FSMainChart(outputPK = extendedData.output.outputPK.get).chart

    fsSetLis5.foreach(fsSet => FSsubHTML.makeHtml(extendedData, fsRunReq, fsSet))

    val content = makeContent(extendedData, fsRunReq, fsSetLis5.sortBy(fsSet => "%020.2f".format(fsSet.jaw090.NominalBeamEnergy) + fsSet.jaw090.isFFF), mainChart.html)
    val javascript = s"""<script src="${FSHistoryRestlet.path}?${FSHistoryRestlet.outputPKTag}=${extendedData.output.outputPK.get}"></script>"""
    val text = Phase2Util.wrapSubProcedure(extendedData, content, FSAnalysis.subProcedureName, ProcedureStatus.done, runScript = Some(javascript), rtimageMap = fsRunReq.rtimageMap)
    val mainHtmlFile = new File(dir, htmlFileName)
    Util.writeBinaryFile(mainHtmlFile, text.getBytes)

    summary(mainHtmlFile, fsSetLis5)
  }

  def main(args: Array[String]): Unit = { // TODO rm
    println("hey")
    println(htmlFileName)
    println(FSMeasure.j)
    println("done")
  }
}
