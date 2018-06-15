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
import org.aqa.db.PositioningCheck
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus
import java.io.File
import org.aqa.Config

object PositioningCheckHTML {
  val htmlFileName = "PositioningCheck.html"

  /**
   * Generate a detailed report and write it to the output directory.  Also write a CSV file.  Return an
   * HTML snippet that serves as a summary and a link to the detailed report.
   */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[PositioningCheck], status: ProcedureStatus.Value): Elem = {

    PositioningCheckCSV.makeCsvFile(extendedData, runReq, resultList)

    val csvFileReference = {
      <a title="Download Positioning Check as CSV File" href={ PositioningCheckCSV.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      <a title="View RT Plan DICOM file" href={ extendedData.dicomHref(runReq.rtplan) }>RT Plan</a>
    }

    class Row(val title: String, name: String, val get: (PositioningCheck) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(psnChk: PositioningCheck) = <td title={ title }>{ get(psnChk) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class RowBeamName(override val title: String, name: String, override val get: (PositioningCheck) => String) extends Row(title, name, get) {
      override def toRow(positioningCheck: PositioningCheck) = {

        val dicomFile = runReq.rtimageMap(positioningCheck.beamName)
        val link = extendedData.dicomHref(dicomFile)
        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(positioningCheck) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new RowBeamName("Name of beam in plan", "Beam Name", (psnChk: PositioningCheck) => psnChk.beamName),
      new Row("Gantry Angle plan minus image in degrees", "Gantry Angle", (psnChk: PositioningCheck) => degree(psnChk.gantryAnglePlanMinusImage_deg)),
      new Row("Collimator Angle plan minus image in degrees", "Collimator Angle", (psnChk: PositioningCheck) => degree(psnChk.collimatorAnglePlanMinusImage_deg)),
      new Row("X1 Jaw plan minus image in mm", "X1 Jaw", (psnChk: PositioningCheck) => jaw(psnChk.x1JawPlanMinusImage_mm)),
      new Row("X2 Jaw plan minus image in mm", "X2 Jaw", (psnChk: PositioningCheck) => jaw(psnChk.x2JawPlanMinusImage_mm)),
      new Row("Y1 Jaw plan minus image in mm", "Y1 Jaw", (psnChk: PositioningCheck) => jaw(psnChk.y1JawPlanMinusImage_mm)),
      new Row("Y2 Jaw plan minus image in mm", "Y2 Jaw", (psnChk: PositioningCheck) => jaw(psnChk.y2JawPlanMinusImage_mm)),
      new Row("Energy plan minus image in kev", "Energy", (psnChk: PositioningCheck) => psnChk.energyPlanMinusImage_kev.toString),
      new Row("Yes if Flattening Filter was present", "FF", (psnChk: PositioningCheck) => if (psnChk.flatteningFilter) "Yes" else "No"),
      new Row("Pass if angles and jaw differences within tolerences", "Status", (psnChk: PositioningCheck) => if (psnChk.pass) "Pass" else "Fail"))

    def positioningCheckTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def positioningCheckToTableRow(positioningCheck: PositioningCheck): Elem = {
      if (positioningCheck.pass) {
        <tr>{ rowList.map(row => row.toRow(positioningCheck)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(positioningCheck)) }</tr>
      }
    }

    val tbody = resultList.map(psnChk => positioningCheckToTableRow(psnChk))

    val content = {
      <div>
        <div class="row" style="margin:20px;">
          <div class="col-md-1">{ csvFileReference }</div>
          <div class="col-md-1">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:20px;">
          <table class="table table-striped">
            { positioningCheckTableHeader }
            <tbody>{ tbody }</tbody>
          </table>
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "Positioning Check", status)
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
            Positioning Check<br/>
            <img src={ iconImage } height="32"/>
          </a>
        </div>
      }
      elem
    }

    makeSummary
  }

}