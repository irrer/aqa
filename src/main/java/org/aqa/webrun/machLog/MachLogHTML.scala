package org.aqa.webrun.machLog

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
  * @param oldMachLogList Machine log entries that were uploaded by the user.  Some of these
  *                            may have been previously uploaded and are already in the database.
  * @param newMaintenanceRecordList New maintenance records created as a result of this upload.
  * @param oldMaintenanceRecordList List of all maintenance records constructed from the uploaded
  *                              machine log list.  Some of these may have already existed in the
  *                              database prior to this upload as a result of a previous upload.
  */
class MachLogHTML(
    extendedData: ExtendedData,
    newMachLogList: Seq[MachineLog],
    oldMachLogList: Seq[MachineLog],
    newMaintenanceRecordList: Seq[MaintenanceRecord],
    oldMaintenanceRecordList: Seq[MaintenanceRecord]
) {

  // private var j = 0 // TODO rm

  private val abbreviationLength = 140
  private def recToHtml(rec: MaintenanceRecord): Elem = {

    // True if this is a new maintenance record, false if it was already in the database.
    val isNew: Boolean = {
      newMaintenanceRecordList.exists(mr => (mr.machineLogPK.get == rec.machineLogPK.get) && (mr.machineLogNodeIndex == rec.machineLogNodeIndex))
    }

    val description = {
      val id = { "MachLog" + rec.maintenanceRecordPK.get }
      // @formatter:off
      val descStart = (rec.summary + " :: " + rec.description).
        replace(WebUtil.nbsp, "").
        replace(WebUtil.gt, "").
        replace("\n", " | ").
        take(abbreviationLength) + " ..."
      // @formatter:on

      def header(button: Elem) = {
        <div>{button}<span style="white-space: pre-line;">{descStart}</span></div>
      }

      val details = {
        <pre>{WebUtil.nl + rec.description.replace("\n", WebUtil.nl)}</pre>
      }

      <td>
        {WebUtil.expandCollapse(id, header, details)}
      </td>
    }

    val creationTime = {
      <td style="white-space: nowrap; vertical-align: top;"><a href={"/admin/MaintenanceRecordUpdate?maintenanceRecordPK=" + rec.maintenanceRecordPK.get}>{rec.creationTime}</a></td>
    }

    val category = {
      if (isNew)
        <td style="white-space: nowrap; vertical-align: top;" class="bg-success">{rec.category}</td>
      else
        <td style="white-space: nowrap; vertical-align: top;">{rec.category}</td>
    }

    <tr>
      {creationTime}
      {category}
      {description}
    </tr>
  }

  private def listMaintenanceRecords(): Elem = {
    val uploaded = (oldMaintenanceRecordList ++ newMaintenanceRecordList).sortBy(_.creationTime.getTime).reverse
    val id = "MachMaintenanceList"
    /*
    <div style="margin:10px" class="container">
      <h3 style="margin-top: 60px;">
        <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#" + id} title="Click to expand details">{WebUtil.amp}#x1F50D;</button>
        Maintenance Records Constructed from Logs: {uploaded.size.toString}<span class="bg-success " style="margin-left:50px;"> {WebUtil.nbsp} New: {newMaintenanceRecordList.size} {WebUtil.nbsp} </span>
      </h3>
      <div id={id} class="collapse">
        <table style="margin:10px;">
          <thead>
            <tr>
              <th>Date</th>
              <th>Category</th>
              <th>Description</th>
            </tr>
          </thead>
          {uploaded.map(recToHtml)}
        </table>
      </div>
    </div>
     */

    def header(button: Elem) = {
      <h3 style="margin-top: 60px;">
        <div>
          {button}
          Maintenance Records Constructed from Logs: {uploaded.size.toString}<span class="bg-success " style="margin-left:50px;"> {WebUtil.nbsp} New: {newMaintenanceRecordList.size} {WebUtil.nbsp} </span>
        </div>
      </h3>
    }

    val details = {
      <table style="margin:10px;">
        <thead>
          <tr>
            <th>Date</th>
            <th>Category</th>
            <th>Summary and Description</th>
          </tr>
        </thead>
        {uploaded.map(recToHtml)}
      </table>
    }

    <div>
      {WebUtil.expandCollapse(id, header, details)}
    </div>
  }

  private def logToHtml(machLog: MachineLog): Elem = {
    val elem = machLog.elem
    val nodeNameList = {
      val textList = (elem \ "Node").map(n => (n \ "@name").text)
      textList.map(name => <p style="white-space: nowrap;">{name}</p>)
    }
    val showHide = {

      val id = { "MachLog" + machLog.machineLogPK.get }

      val contentStart = {
        (elem \ "Node").map(Util.prettyPrint).mkString("  ").take(abbreviationLength) + " ..."
      }

      val text: Elem = {
        val t = Util.escapeXml(WebUtil.nl + machLog.content).replace("&", WebUtil.amp)
        <pre>{t}</pre>
      }

      def header(button: Elem) = <div>{button}<span style="white-space: pre-line;">{contentStart}</span></div>

      WebUtil.expandCollapse(id, header, text)
    }

    val isNew = newMachLogList.exists(ml => ml.DateTimeSaved.getTime == machLog.DateTimeSaved.getTime)

    val categoryClass = if (isNew) "bg-success" else ""

    <tr>
      <td style="vertical-align: top;">{machLog.DateTimeSaved}</td>
      <td style="vertical-align: top;" class={categoryClass}>{nodeNameList}</td>
      <td>{showHide}</td>
    </tr>
  }

  private def listMachineLogs(): Elem = {
    val list = (oldMachLogList ++ newMachLogList).sortBy(_.DateTimeSaved.getTime).reverse
    val id = "MachLogList"

    val details = {
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
    }

    /*
    <div style="margin:10px" class="container">
      <h3>
        <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#" + id} title="Click to expand details">{WebUtil.amp}#x1F50D;</button>
        Machine Logs Uploaded: {list.size.toString} <span class="bg-success " style="margin-left:50px;"> {WebUtil.nbsp} New: {newMachLogList.size} {WebUtil.nbsp} </span>
      </h3>
      <div id={id} class="collapse">
        {details}
      </div>
    </div>
     */

    def header(button: Elem) = {
      <h3>
        {button}
        Machine Logs Uploaded: {list.size.toString} <span class="bg-success " style="margin-left:50px;"> {WebUtil.nbsp} New: {newMachLogList.size} {WebUtil.nbsp} </span>
      </h3>
    }

    WebUtil.expandCollapse(id, header, details)
  }

  private def content(): Elem = {
    //noinspection SpellCheckingInspection
    <div class="row" style="hey:6;">
      <style>{" th, td { padding: 5px; border:1px solid lightgrey;}"}</style>
      <div>{listMaintenanceRecords()}</div>
      <div style="margin: 50px;"> </div>
      <div>{listMachineLogs()}</div>
    </div>
  }

  def generate(): Unit = {
    val elem = content()
    val c2 = ExtendedData.wrapExtendedData(extendedData, elem, offset = 0)
    val text = WebUtil.wrapBody(c2, "Machine Log")
    val file = new File(extendedData.output.dir, "display.html")
    Util.writeFile(file, text)
  }

}
