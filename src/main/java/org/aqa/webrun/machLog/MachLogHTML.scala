package org.aqa.webrun.machLog

import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.db.MachineLog
import org.aqa.db.MaintenanceRecord
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import java.io.File
import scala.xml.Elem

/**
  * Generate an HTML page to show what happened.
  *
  * @param extendedData Meta data
  * @param newMachLogList Machine log entries that were not previously in the database.
  * @param uploadedMachLogList Machine log entries that were uploaded by the user.  Some of these
  *                            may have been previously uploaded and are already in the database.
  * @param newMaintenanceRecordList New maintenance records created as a result of this upload.
  * @param maintenanceRecordList List of all maintenance records constructed from the uploaded
  *                              machine log list.  Some of these may have already existed in the
  *                              database prior to this upload as a result of a previous upload.
  */
class MachLogHTML(
    extendedData: ExtendedData,
    newMachLogList: Seq[MachineLog],
    uploadedMachLogList: Seq[MachineLog],
    newMaintenanceRecordList: Seq[MaintenanceRecord],
    maintenanceRecordList: Seq[MaintenanceRecord]
) {

  private var j = 0 // TODO rm

  private val abbreviationLength = 140
  private def recToHtml(rec: MaintenanceRecord): Elem = {

    val description = {
      // <td><a href={"/admin/MaintenanceRecordUpdate?maintenanceRecordPK=" + rec.maintenanceRecordPK.get}>{rec.summary}</a></td> // TODO put back
      j = j + 1
      // val id = { "MachLog" + machLog.machineLogPK }
      val descStart = rec.description.take(abbreviationLength).replace("\n", " | ") + " ..."

      val id = { "Rec" + j }
      <div class="container">
        <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#" + id}>{WebUtil.amp}#x1F50D;</button>
        <span style="white-space: pre-line;">{descStart}</span>
        <div id={id} class="collapse">
          <pre>{WebUtil.nl + rec.description.replace("\n", WebUtil.nl)}</pre>
        </div>
      </div>
    }

    <tr>
      <td>{rec.creationTime}</td>
      <td style="white-space: nowrap;"><a href={"/admin/MaintenanceRecordUpdate?maintenanceRecordPK=" + j}>{rec.category}</a></td>
      <td>{description}</td>
    </tr>
  }

  private def listMaintenanceRecords(title: String, list: Seq[MaintenanceRecord]): Elem = {
    <div style="margin:10px">
      <h3>{title}</h3>
      <table style="margin:10px;">
        <thead>
          <tr>
            <th>Date</th>
            <th>Category</th>
            <th>Description</th>
          </tr>
        </thead>
        {list.map(recToHtml)}
      </table>
    </div>
  }

  private def logToHtml(machLog: MachineLog): Elem = {
    val elem = machLog.elem
    val nodeNameList = {
      val textList = (elem \ "Node").map(n => (n \ "@name").text)
      textList.map(name => <p style="white-space: nowrap;">{name}</p>)
    }
    val showHide = {

      // val id = { "MachLog" + machLog.machineLogPK }
      j = j + 1
      val id = { "MachLog" + j }

      val contentStart = {
        (elem \ "Node").map(Util.prettyPrint).mkString("  ").take(abbreviationLength) + " ..."
      }

      val j4: Elem = { <pre>{WebUtil.nl + machLog.content}</pre> }

      <div class="container">
        <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#" + id}>{WebUtil.amp}#x1F50D;</button>
        <span style="white-space: pre-line;">{contentStart}</span>
        <div id={id} class="collapse">
          {j4}
        </div>
      </div>
    }
    <tr>
      <td>{machLog.DateTimeSaved}</td>
      <td>{nodeNameList}</td>
      <td>{showHide}</td>
    </tr>
  }

  private def listMachineLogs(title: String, list: Seq[MachineLog]): Elem = {
    <div style="margin:10px">
      <h3>{title}</h3>
        <table style="margin:10px;">
        <thead>
          <tr>
            <th>Date</th>
            <th>Category(ies)</th>
            <th>XML</th>
          </tr>
        </thead>
        {list.map(logToHtml)}
      </table>
    </div>
  }

  private def content(): Elem = {
    // val preexistingMaintenanceRecords =
    <div class="row">
      <style>{" th, td { padding: 5px; border:1px solid lightgrey;}"}</style>
      <div>
        {listMaintenanceRecords("New Maintenance Records: " + newMaintenanceRecordList.size, newMaintenanceRecordList)}
      </div>
      <div style="margin: 50px;"> </div>
      <div>
        {listMachineLogs("New Machine Logs: " + newMachLogList.size, newMachLogList)}
      </div>
    </div>
  }

  def generate(): Unit = {
    val elem = content()
    val c2 = ExtendedData.wrapExtendedData(extendedData, elem)
    val text = WebUtil.wrapBody(c2, "Machine Log")
    Trace.trace("======================= Full text:\n" + text)
    val file = new File(extendedData.output.dir, "display.html")
    Util.writeFile(file, text)
  }

}
