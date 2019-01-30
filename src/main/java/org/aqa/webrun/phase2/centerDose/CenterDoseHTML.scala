package org.aqa.webrun.phase2.centerDose

import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.Institution
import org.aqa.db.Input
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.Util
import java.util.Date
import org.aqa.web.WebServer
import org.aqa.db.CenterDose
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus
import java.io.File
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeList
import java.awt.Point
import com.pixelmed.dicom.TagFromName
import org.aqa.Logging
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.IsoImagePlaneTranslator

object CenterDoseHTML extends Logging {
  private val htmlFileName = "CenterDose.html"

  /**
   * Generate a detailed report and write it to the output directory.  Return an
   * HTML snippet that serves as a summary and a link to the detailed report.
   */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[CenterDose], status: ProcedureStatus.Value): Elem = {

    val planHref = Phase2Util.dicomViewHref(runReq.rtplan.attributeList.get, extendedData, runReq)
    val viewRtPlan = {
      <a title="View RT Plan DICOM file" href={ planHref }>RT Plan</a>
    }

    /**
     * Get the history of results from previous runs of this procedure.  Exclude results that were run on this exact data.
     */
    val history = {
      val uidSet = resultList.map(cd => cd.SOPInstanceUID).toSet
      CenterDose.recentHistory(Config.CenterDoseReportedHistoryLimit, extendedData.machine.machinePK.get, extendedData.procedure.procedurePK.get, extendedData.output.dataDate).
        filter(cd => !uidSet.contains(cd.SOPInstanceUID))
    }

    val resultListAsHistory = resultList.map(cd => new CenterDose.CenterDoseHistory(extendedData.output.dataDate.get, cd.beamName, cd.dose, cd.SOPInstanceUID))
    val units = runReq.flood.attributeList.get.get(TagFromName.RescaleType).getSingleStringValueOrDefault("CU")

    val chart = new CenterDoseChart(resultListAsHistory, history, units, extendedData)

    class Column(val title: String, columnName: String, val get: (CenterDose) => String) {
      def toHeader = <th title={ title }>{ columnName }</th>
      def toRow(cntrDose: CenterDose) = <td title={ title }>{ get(cntrDose) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class ColumnBeamName(override val title: String, columnName: String, override val get: (CenterDose) => String) extends Column(title, columnName, get) {
      override def toRow(centerDose: CenterDose) = {
        val dicomFile = if (centerDose.beamName.equals(Config.FloodFieldBeamName)) runReq.flood else runReq.rtimageMap(centerDose.beamName)
        val link = Phase2Util.dicomViewHref(dicomFile.attributeList.get, extendedData, runReq)
        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(centerDose) }</a></td> }
        elem
      }
    }

    class ColumnChart(override val title: String, columnName: String, override val get: (CenterDose) => String) extends Column(title, columnName, get) {
      override def toHeader = <th title={ title }>History</th>
      override def toRow(cntrDose: CenterDose) = <td title={ title } id={ chart.refOfBeam(cntrDose.beamName) }/>
    }

    val rowList = Seq(
      new ColumnBeamName("Name of beam in plan", "Beam Name", (centerDose: CenterDose) => centerDose.beamName),
      new Column("Dose", "Dose", (centerDose: CenterDose) => centerDose.dose.formatted("%f")),
      new ColumnChart("History of recent values for this machine and beam", "History", (centerDose: CenterDose) => centerDose.dose.formatted("%f")))

    def centerDoseTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def centerDoseToTableRow(centerDose: CenterDose): Elem = {
      <tr>{ rowList.map(row => row.toRow(centerDose)) }</tr>
    }

    def imageToHtml(centerDose: CenterDose): Elem = {
      <div class="row">
        <h3>{ centerDose.beamName + " : " }<font color='orange'>{ centerDose.dose.formatted("%6.4f") }</font> { units }</h3>
        { chart.refOfBeam(centerDose.beamName) }
      </div>
    }

    val tbody = resultList.map(psnChk => centerDoseToTableRow(psnChk))

    //              <table class="table table-striped">
    //                { centerDoseTableHeader }
    //                <tbody>{ tbody }</tbody>
    //              </table>

    val content = {
      <div>
        <div class="row" style="margin:50px;">
          <div class="col-md-1">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:50px;">
          <div class="row" style="margin:50px;">
            <div class="col-md-12">
              { resultList.map(cd => imageToHtml(cd)) }
            </div>
          </div>
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "Center Dose", status, Some(chart.chartScript), runReq)
    val file = new File(extendedData.output.dir, htmlFileName)
    Util.writeBinaryFile(file, text.getBytes)

    /**
     * Make a tiny summary and link to the detailed report.
     */
    def makeSummary = {
      val elem = {
        <div title="Click for details.">
          <a href={ htmlFileName }>
            Center Dose.  Images:{ resultList.size }<br/>
            <img src={ Config.passImageUrl } height="32"/>
          </a>
        </div>
      }
      elem
    }
    makeSummary
  }

  private def analyse(extendedData: ExtendedData, runReq: RunReq): Seq[CenterDose] = {
    val pointList = Phase2Util.makeCenterDosePointList(runReq.flood.attributeList.get)
    val outputPK = extendedData.output.outputPK.get

    /**
     * Average the pixels at the given points.
     */
    def avg(dicomImage: DicomImage): Double = {
      pointList.map(p => dicomImage.get(p.getX.toInt, p.getY.toInt)).sum / pointList.size
    }

    /**
     * Construct a CenterDose
     */
    def measure(beamName: String, dicomImage: DicomImage, attributeList: AttributeList): CenterDose = {
      val m = attributeList.get(TagFromName.RescaleSlope).getDoubleValues().head
      val b = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues().head
      val dose = (avg(dicomImage) * m) + b
      val SOPInstanceUID = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
      val units = attributeList.get(TagFromName.RescaleType).getSingleStringValueOrEmptyString
      new CenterDose(None, outputPK, SOPInstanceUID, beamName, dose, units)
    }

    val centerDoseFlood = measure(Config.FloodFieldBeamName, runReq.floodOriginalImage, runReq.flood.attributeList.get)
    val availableBeamList = Config.CenterDoseBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))
    val centerDoseList = availableBeamList.map(beamName => measure(beamName, runReq.derivedMap(beamName).originalImage, runReq.rtimageMap(beamName).attributeList.get))
    centerDoseFlood +: centerDoseList
  }

  private val subProcedureName = "Center Dose"

  class CenterDoseResult(summary: Elem, status: ProcedureStatus.Value, resultList: Seq[CenterDose]) extends SubProcedureResult(summary, status, subProcedureName)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, CenterDoseResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CenterDose")
      val status = ProcedureStatus.done
      val resultList = analyse(extendedData, runReq)
      logger.info("Storing results for " + resultList.size + " CenterDose rows")
      logger.info("CenterDose results: " + resultList.mkString("\n"))
      CenterDose.insert(resultList)
      val summary = makeDisplay(extendedData, runReq, resultList, status)
      val result = Right(new CenterDoseResult(summary, status, resultList))
      logger.info("Finished analysis of CenterDose")
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CenterDose: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }

}
