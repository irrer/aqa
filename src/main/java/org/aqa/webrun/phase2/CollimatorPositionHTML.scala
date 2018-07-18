package org.aqa.webrun.phase2

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
import org.aqa.db.CollimatorPosition
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus
import java.io.File
import org.aqa.Config

object CollimatorPositionHTML {
  val htmlFileName = "CollimatorPosition.html"

  /**
   * Generate a detailed report and write it to the output directory.  Also write a CSV file.  Return an
   * HTML snippet that serves as a summary and a link to the detailed report.
   */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[CollimatorPosition], crashList: Seq[String], status: ProcedureStatus.Value): Elem = {

    CollimatorPositionCSV.makeCsvFile(extendedData, runReq, resultList)

    val csvFileReference = {
      <a title={ "Download " + CollimatorPositionAnalysis.subProcedureName + " as CSV File" } href={ CollimatorPositionCSV.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      <a title="View RT Plan DICOM file" href={ extendedData.dicomHref(runReq.rtplan) }>RT Plan</a>
    }

    class Col(val title: String, name: String, val get: (CollimatorPosition) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(psnChk: CollimatorPosition) = <td title={ title }>{ get(psnChk) }</td>
    }

    def degree(deg: Double): String = (Util.modulo360(deg).round.toInt % 360).formatted("%4d")

    def leaf(diff: Double): String = diff.formatted("%8.3f")

    class ColBeamName(override val title: String, name: String, override val get: (CollimatorPosition) => String) extends Col(title, name, get) {
      override def toRow(collimatorPosition: CollimatorPosition) = {

        val dicomFile = runReq.rtimageMap(collimatorPosition.beamName)
        val link = extendedData.dicomHref(dicomFile)
        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(collimatorPosition) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new ColBeamName("Name of beam in plan", "Beam", (psnChk: CollimatorPosition) => psnChk.beamName),
      new Col("Flood field compensation used: True/False", "Flood Comp.", (psnChk: CollimatorPosition) => (if (psnChk.FloodCompensation) "T" else "F")),
      new Col("Gantry Angle in degrees", "Gantry", (psnChk: CollimatorPosition) => degree(psnChk.gantryAngle_deg)),
      new Col("Collimator Angle in degrees", "Collimator", (psnChk: CollimatorPosition) => degree(psnChk.collimatorAngle_deg)),
      new Col("Expected X1 - X1 edge in image, in mm", "X1", (psnChk: CollimatorPosition) => leaf(psnChk.X1_ExpectedMinusImage_mm)),
      new Col("Expected X2 - X2 edge in image, in mm", "X2", (psnChk: CollimatorPosition) => leaf(psnChk.X2_ExpectedMinusImage_mm)),
      new Col("Expected Y1 - Y1 edge in image, in mm", "Y1", (psnChk: CollimatorPosition) => leaf(psnChk.Y1_ExpectedMinusImage_mm)),
      new Col("Expected Y2 - Y2 edge in image, in mm", "Y2", (psnChk: CollimatorPosition) => leaf(psnChk.Y2_ExpectedMinusImage_mm)),
      new Col("Pass if angles and jaw differences within tolerences", "Status", (psnChk: CollimatorPosition) => if (psnChk.status.toString.equals(ProcedureStatus.pass.toString)) "Pass" else "Fail"))

    def collimatorPositionTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def collimatorPositionToTableRow(collimatorPosition: CollimatorPosition): Elem = {
      if (collimatorPosition.status.toString.equals(ProcedureStatus.pass.toString)) {
        <tr>{ rowList.map(row => row.toRow(collimatorPosition)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(collimatorPosition)) }</tr>
      }
    }

    val tbody = resultList.map(psnChk => collimatorPositionToTableRow(psnChk))

    val crashTable = {
      def stringToRow(s: String) = { <tr><td>{ s }</td></tr> }
      <div>
        <h3>Crashed Beams</h3>
        <table class="table table-striped">
          <tbody>{ crashList.map(c => stringToRow(c)) }</tbody>
        </table>
      </div>
    }

    val content = {
      <div>
        <div class="row" style="margin:20px;">
          <div class="col-md-1">{ csvFileReference }</div>
          <div class="col-md-1">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:20px;">
          <table class="table table-striped">
            { collimatorPositionTableHeader }
            <tbody>{ tbody }</tbody>
          </table>
          { if (crashList.nonEmpty) crashTable }
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, CollimatorPositionAnalysis.subProcedureName, status, None)
    val file = new File(extendedData.output.dir, htmlFileName)
    Util.writeBinaryFile(file, text.getBytes)

    /**
     * Make a tiny summary and link to the detailed report.
     */
    def makeSummary = {
      val iconImage = if (status == ProcedureStatus.pass) Config.passImageUrl else Config.failImageUrl
      val elem = {
        <div title="Click for details.">
          <a href={ htmlFileName }>
            Collimator Position<br/>
            <img src={ iconImage } height="32"/>
          </a>
        </div>
      }
      elem
    }

    makeSummary
  }

}