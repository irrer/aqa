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
import org.aqa.db.ImageIdentification
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil

object CheckAnglesHTML {

  private def makeDisplay(output: Output, runReq: CheckAnglesRunRequirements) = {

    def wrap(col: Int, elem: Elem): Elem = {
      <div class={ "col-md-" + col }>{ elem }</div>
    }

    def wrap2(col: Int, name: String, value: String): Elem = {
      <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ value }</div>
    }

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

    def dateToString(date: Option[Date]): String = {
      date match {
        case Some(date) => Util.timeHumanFriendly(date)
        case _ => "unknown"
      }
    }

    val machineId = if (machine.isDefined) machine.get.id else "unknown"
    val userId = if (user.isDefined) user.get.id else "unknown"

    val elapsed: String = {
      val fin = output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _ => System.currentTimeMillis
      }
      val elapsed = fin - output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = {
      procedure match {
        case Some(proc) =>
          proc.name + " : " + proc.version
        case _ => ""
      }
    }

    CheckAnglesCSV.makeCsvFile(
      procedureDesc,
      institutionName,
      output.dir,
      machineId,
      (if (output.dataDate.isDefined) Util.standardDateFormat.format(output.dataDate.get) else "none"),
      (Util.standardDateFormat.format(if (output.analysisDate.isDefined) output.analysisDate.get else (new Date))),
      userId,
      runReq)

    val csvFileReference = {
      <a title="Download Image Identification as CSV File" href={ ImageIdentification.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      val title = "RT Plan"
      val link = DicomAccess.write(runReq.plan, WebServer.urlOfMachineConfigurationFile(runReq.plan.file), title, output.dir, DicomFile.ContrastModel.maxContrast)
      val elem = { <a title="View RT Plan DICOM file" href={ link }>{ title }</a> }
      elem
    }

    class Row(val title: String, name: String, val get: (ImageIdentificationFile) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(imgId: ImageIdentificationFile) = <td title={ title }>{ get(imgId) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class RowBeamName(override val title: String, name: String, override val get: (ImageIdentificationFile) => String) extends Row(title, name, get) {
      override def toRow(imgIdFile: ImageIdentificationFile) = {

        val link = DicomAccess.write(imgIdFile.dicomFile, WebServer.urlOfResultsFile(imgIdFile.dicomFile.file), get(imgIdFile) + " : " + imgIdFile.dicomFile.file.getName, output.dir, DicomFile.ContrastModel.maxContrast)

        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(imgIdFile) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new RowBeamName("Name of beam in plan", "Beam Name", (imgIdFile: ImageIdentificationFile) => imgIdFile.imageIdentification.beamName),
      new Row("Gantry Angle plan minus image in degrees", "Gantry Angle", (imgIdFile: ImageIdentificationFile) => degree(imgIdFile.imageIdentification.gantryAnglePlanMinusImage_deg)),
      new Row("Collimator Angle plan minus image in degrees", "Collimator Angle", (imgIdFile: ImageIdentificationFile) => degree(imgIdFile.imageIdentification.collimatorAnglePlanMinusImage_deg)),
      new Row("X1 Jaw plan minus image in mm", "X1 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.x1JawPlanMinusImage_mm)),
      new Row("X2 Jaw plan minus image in mm", "X2 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.x2JawPlanMinusImage_mm)),
      new Row("Y1 Jaw plan minus image in mm", "Y1 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.y1JawPlanMinusImage_mm)),
      new Row("Y2 Jaw plan minus image in mm", "Y2 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.y2JawPlanMinusImage_mm)),
      new Row("Energy plan minus image in kev", "Energy", (imgIdFile: ImageIdentificationFile) => imgIdFile.imageIdentification.energyPlanMinusImage_kev.toString),
      new Row("Yes if Flattening Filter was present", "FF", (imgIdFile: ImageIdentificationFile) => if (imgIdFile.imageIdentification.flatteningFilter) "Yes" else "No"),
      new Row("Pass if angles and jaw differences within tolerences", "Status", (imgIdFile: ImageIdentificationFile) => if (imgIdFile.imageIdentification.pass) "Pass" else "Fail"))

    def imageIdentificationTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def imageIdentificationToTableRow(imgIdFile: ImageIdentificationFile): Elem = {
      if (imgIdFile.imageIdentification.pass) {
        <tr>{ rowList.map(row => row.toRow(imgIdFile)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(imgIdFile)) }</tr>
      }
    }

    val passFailImage = {
      val j = runReq
      if (runReq.pass) {
        <div title="Passed!"><img src="/static/images/pass.png" width="128"/></div>
      } else {
        <div title="Failed"><img src="/static/images/fail.png" width="128"/></div>
      }
    }

    val tbody = runReq.imageIdFileList.sortWith((a, b) => DicomUtil.compareDicom(a.dicomFile.attributeList.get, b.dicomFile.attributeList.get) < 0).map(iif => imageIdentificationToTableRow(iif))

    val div = {
      <div class="row col-md-10 col-md-offset-1">
        <div class="row">
          <div class="col-md-1">{ passFailImage }</div>
          <div class="col-md-3" title="Image Identification"><h2>Image Identification</h2></div>
          <div class="col-md-1" title="Machine"><h2>{ machineId }</h2></div>
        </div>
        <div class="row" style="margin:20px;">
          { wrap2(1, "Institution", institutionName) }
          { wrap2(2, "Data Acquisition", dateToString(output.dataDate)) }
          { wrap2(2, "Analysis", analysisDate) }
          { wrap2(1, "User", userId) }
          { wrap2(1, "Elapsed", elapsed) }
          { wrap2(3, "Procedure", procedureDesc) }
        </div>
        <div class="row" style="margin:20px;">
          <div class="col-md-1">{ csvFileReference }</div>
          <div class="col-md-1">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:20px;">
          <table class="table table-striped">
            { imageIdentificationTableHeader }
            <tbody>{ tbody }</tbody>
          </table>
        </div>
      </div>
    }

    val text = wrapBody(div, "Image Identification", None, true, None)
    text
  }

}