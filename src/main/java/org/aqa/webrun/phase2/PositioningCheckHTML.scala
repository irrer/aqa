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
   * Associate results with file.
   */
  case class PositioningCheckFile(dicomFile: DicomFile, positioningCheck: PositioningCheck);

  /**
   * Generate a detailed report and write it to the output directory.  Also write a CSV file.  Return an
   * HTML snippet that serves as a summary and a link to the detailed report.
   */
  def makeDisplay(output: Output, runReq: RunReq, resultList: Seq[PositioningCheckFile], status: ProcedureStatus.Value): Elem = {

    val machine = if (output.machinePK.isDefined) Machine.get(output.machinePK.get) else None
    val institution = if (machine.isDefined) Institution.get(machine.get.institutionPK) else None
    val input = Input.get(output.inputPK)
    val procedure = Procedure.get(output.procedurePK)
    val user = if (output.userPK.isDefined) User.get(output.userPK.get) else None
    val institutionName = if (institution.isDefined) institution.get.name else "unknown"

    val analysisDate: String = {
      val date = output.analysisDate match {
        case Some(d) => d
        case _ => output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    val machineId = if (machine.isDefined) machine.get.id else "unknown"
    val userId = if (user.isDefined) user.get.id else "unknown"

    PositioningCheckCSV.makeCsvFile(output, runReq, resultList.map(r => r.positioningCheck))

    val csvFileReference = {
      <a title="Download Positioning Check as CSV File" href={ PositioningCheckCSV.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      val title = "RT Plan"
      val link = DicomAccess.write(runReq.rtplan, WebServer.urlOfResultsFile(runReq.rtplan.file), title, output.dir, DicomFile.ContrastModel.maxContrast)
      val elem = { <a title="View RT Plan DICOM file" href={ link }>{ title }</a> }
      elem
    }

    class Row(val title: String, name: String, val get: (PositioningCheckFile) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(imgId: PositioningCheckFile) = <td title={ title }>{ get(imgId) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class RowBeamName(override val title: String, name: String, override val get: (PositioningCheckFile) => String) extends Row(title, name, get) {
      override def toRow(imgIdFile: PositioningCheckFile) = {

        val link = DicomAccess.write(imgIdFile.dicomFile, WebServer.urlOfResultsFile(imgIdFile.dicomFile.file), get(imgIdFile) + " : " + imgIdFile.dicomFile.file.getName, output.dir, DicomFile.ContrastModel.maxContrast)

        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(imgIdFile) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new RowBeamName("Name of beam in plan", "Beam Name", (imgIdFile: PositioningCheckFile) => imgIdFile.positioningCheck.beamName),
      new Row("Gantry Angle plan minus image in degrees", "Gantry Angle", (imgIdFile: PositioningCheckFile) => degree(imgIdFile.positioningCheck.gantryAnglePlanMinusImage_deg)),
      new Row("Collimator Angle plan minus image in degrees", "Collimator Angle", (imgIdFile: PositioningCheckFile) => degree(imgIdFile.positioningCheck.collimatorAnglePlanMinusImage_deg)),
      new Row("X1 Jaw plan minus image in mm", "X1 Jaw", (imgIdFile: PositioningCheckFile) => jaw(imgIdFile.positioningCheck.x1JawPlanMinusImage_mm)),
      new Row("X2 Jaw plan minus image in mm", "X2 Jaw", (imgIdFile: PositioningCheckFile) => jaw(imgIdFile.positioningCheck.x2JawPlanMinusImage_mm)),
      new Row("Y1 Jaw plan minus image in mm", "Y1 Jaw", (imgIdFile: PositioningCheckFile) => jaw(imgIdFile.positioningCheck.y1JawPlanMinusImage_mm)),
      new Row("Y2 Jaw plan minus image in mm", "Y2 Jaw", (imgIdFile: PositioningCheckFile) => jaw(imgIdFile.positioningCheck.y2JawPlanMinusImage_mm)),
      new Row("Energy plan minus image in kev", "Energy", (imgIdFile: PositioningCheckFile) => imgIdFile.positioningCheck.energyPlanMinusImage_kev.toString),
      new Row("Yes if Flattening Filter was present", "FF", (imgIdFile: PositioningCheckFile) => if (imgIdFile.positioningCheck.flatteningFilter) "Yes" else "No"),
      new Row("Pass if angles and jaw differences within tolerences", "Status", (imgIdFile: PositioningCheckFile) => if (imgIdFile.positioningCheck.pass) "Pass" else "Fail"))

    def positioningCheckTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def positioningCheckToTableRow(imgIdFile: PositioningCheckFile): Elem = {
      if (imgIdFile.positioningCheck.pass) {
        <tr>{ rowList.map(row => row.toRow(imgIdFile)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(imgIdFile)) }</tr>
      }
    }

    val tbody = resultList.toSeq.sortWith((a, b) => DicomUtil.compareDicom(a.dicomFile.attributeList.get, b.dicomFile.attributeList.get) < 0).map(iif => positioningCheckToTableRow(iif))

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
    val text = Phase2Util.wrapSubProcedure(output, content, "Positioning Check", status)
    val file = new File(output.dir, htmlFileName)
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