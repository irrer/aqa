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
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[CollimatorPosition], status: ProcedureStatus.Value): Elem = {

    CollimatorPositionCSV.makeCsvFile(extendedData, runReq, resultList)

    val csvFileReference = {
      <a title="Download Collimator Position as CSV File" href={ CollimatorPositionCSV.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      <a title="View RT Plan DICOM file" href={ extendedData.dicomHref(runReq.rtplan) }>RT Plan</a>
    }

    class Row(val title: String, name: String, val get: (CollimatorPosition) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(psnChk: CollimatorPosition) = <td title={ title }>{ get(psnChk) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class RowBeamName(override val title: String, name: String, override val get: (CollimatorPosition) => String) extends Row(title, name, get) {
      override def toRow(collimatorPosition: CollimatorPosition) = {

        val dicomFile = runReq.rtimageMap(collimatorPosition.beamName)
        val link = extendedData.dicomHref(dicomFile)
        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(collimatorPosition) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new RowBeamName("Name of beam in plan", "Beam Name", (psnChk: CollimatorPosition) => psnChk.beamName),
      new Row("Actual Gantry Angle in degrees", "Gantry Angle", (psnChk: CollimatorPosition) => degree(psnChk.gantryAngle_deg)),
      new Row("Actual Collimator Angle in degrees", "Collimator Angle", (psnChk: CollimatorPosition) => degree(psnChk.collimatorAngle_deg)),
      new Row("Planned north edge - north edge in image, in mm", "North", (psnChk: CollimatorPosition) => jaw(psnChk.northPlanMinusImage_mm)),
      new Row("Planned south edge - north edge in image, in mm", "South", (psnChk: CollimatorPosition) => jaw(psnChk.southPlanMinusImage_mm)),
      new Row("Planned east edge - north edge in image, in mm", "East", (psnChk: CollimatorPosition) => jaw(psnChk.eastPlanMinusImage_mm)),
      new Row("Planned west edge - north edge in image, in mm", "West", (psnChk: CollimatorPosition) => jaw(psnChk.westPlanMinusImage_mm)),
      new Row("Pass if angles and jaw differences within tolerences", "Status", (psnChk: CollimatorPosition) => if (psnChk.status.toString.equals(ProcedureStatus.pass)) "Pass" else "Fail"))

    def collimatorPositionTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def collimatorPositionToTableRow(collimatorPosition: CollimatorPosition): Elem = {
      if (collimatorPosition.status.toString.equals(ProcedureStatus.pass)) {
        <tr>{ rowList.map(row => row.toRow(collimatorPosition)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(collimatorPosition)) }</tr>
      }
    }

    val tbody = resultList.map(psnChk => collimatorPositionToTableRow(psnChk))

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
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "Collimator Position", status, None)
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