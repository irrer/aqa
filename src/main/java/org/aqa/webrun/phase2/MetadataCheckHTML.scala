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
import org.aqa.db.MetadataCheck
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus
import java.io.File
import org.aqa.Config

object MetadataCheckHTML {
  val htmlFileName = "MetadataCheck.html"

  /**
   * Generate a detailed report and write it to the output directory.  Also write a CSV file.  Return an
   * HTML snippet that serves as a summary and a link to the detailed report.
   */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[MetadataCheck], status: ProcedureStatus.Value): Elem = {

    MetadataCheckCSV.makeCsvFile(extendedData, runReq, resultList)

    val csvFileReference = {
      <a title="Download Metadata Check as CSV File" href={ MetadataCheckCSV.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      val planHref = Phase2Util.dicomViewHref(runReq.rtplan.attributeList.get, extendedData, runReq)
      <a title="View RT Plan DICOM file" href={ planHref }>RT Plan</a>
    }

    class Col(val title: String, name: String, val get: (MetadataCheck) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(psnChk: MetadataCheck) = <td title={ title }>{ get(psnChk) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%9.4f")

    def jaw(diff: Double): String = diff.formatted("%12.9f")

    class ColBeamName(override val title: String, name: String, override val get: (MetadataCheck) => String) extends Col(title, name, get) {
      override def toRow(metadataCheck: MetadataCheck) = {

        val dicomFile = runReq.rtimageMap(metadataCheck.beamName)
        val href = Phase2Util.dicomViewHref(dicomFile.attributeList.get, extendedData, runReq)
        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ href }>{ get(metadataCheck) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new ColBeamName("Name of beam in plan", "Beam Name", (psnChk: MetadataCheck) => psnChk.beamName),
      new Col("Gantry Angle plan minus image in degrees", "Gantry Angle", (psnChk: MetadataCheck) => degree(psnChk.gantryAnglePlanMinusImage_deg)),
      new Col("Collimator Angle plan minus image in degrees", "Collimator Angle", (psnChk: MetadataCheck) => degree(psnChk.collimatorAnglePlanMinusImage_deg)),
      new Col("X1 Jaw plan minus image in mm", "X1 Jaw", (psnChk: MetadataCheck) => jaw(psnChk.x1JawPlanMinusImage_mm)),
      new Col("X2 Jaw plan minus image in mm", "X2 Jaw", (psnChk: MetadataCheck) => jaw(psnChk.x2JawPlanMinusImage_mm)),
      new Col("Y1 Jaw plan minus image in mm", "Y1 Jaw", (psnChk: MetadataCheck) => jaw(psnChk.y1JawPlanMinusImage_mm)),
      new Col("Y2 Jaw plan minus image in mm", "Y2 Jaw", (psnChk: MetadataCheck) => jaw(psnChk.y2JawPlanMinusImage_mm)),
      new Col("Energy plan minus image in kev", "Energy", (psnChk: MetadataCheck) => psnChk.energyPlanMinusImage_kev.toString),
      new Col("Yes if Flattening Filter was present", "FF", (psnChk: MetadataCheck) => if (psnChk.flatteningFilter) "Yes" else "No"),
      new Col("Pass if angles and jaw differences within tolerences", "Status", (psnChk: MetadataCheck) => if (psnChk.pass) "Pass" else "Fail"))

    def metadataCheckTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def metadataCheckToTableRow(metadataCheck: MetadataCheck): Elem = {
      if (metadataCheck.pass) {
        <tr>{ rowList.map(row => row.toRow(metadataCheck)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(metadataCheck)) }</tr>
      }
    }

    val tbody = resultList.map(psnChk => metadataCheckToTableRow(psnChk))

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row" style="margin:20px;">
          <div class="col-md-1">{ csvFileReference }</div>
          <div class="col-md-1">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:20px;">
          <table class="table table-striped">
            { metadataCheckTableHeader }
            <tbody>{ tbody }</tbody>
          </table>
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "Metadata Check", status, None, runReq)
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
            Metadata Check<br/>
            <img src={ iconImage } height="32"/>
          </a>
        </div>
      }
      elem
    }

    makeSummary
  }

}
