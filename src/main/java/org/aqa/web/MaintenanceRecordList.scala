package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.MaintenanceRecord
import java.sql.Timestamp
import org.aqa.web.WebUtil._

object MaintenanceRecordList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MaintenanceRecordList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class MaintenanceRecordList extends GenericList[MaintenanceRecord] with WebUtil.SubUrlAdmin {
    override def listName = "MaintenanceRecord"

    /**
     * If a machinePK is given, then filter on that, otherwise list maintenance records for all machines.
     */
    override def getData(valueMap: ValueMapT) = {
        valueMap.get(MachineUpdate.machinePKTag) match {
            case Some(machinePK) => MaintenanceRecord.getByMachine(machinePK.toLong)
            case _ => MaintenanceRecord.list
        }
    }

    override def getPK(value: MaintenanceRecord): Long = value.maintenanceRecordPK.get

    private def descHTML(maintenanceRecord: MaintenanceRecord): Elem = <div>{ WebUtil.firstPartOf(maintenanceRecord.description, 60) }</div>

    private val dateTimeCol = new Column[MaintenanceRecord]("Date/Time", _.dateTime.toString)

    private val userCol = new Column[MaintenanceRecord]("User", (a, b) => (a.dateTime.getTime < b.dateTime.getTime), (user: MaintenanceRecord) => makePrimaryKeyHtml("user", Some(user.userPK))) //(user:MaintenanceRecord) => makePrimaryKeyHtml("user", Some(user.userPK)))

    private val summaryCol = new Column[MaintenanceRecord]("Summary", _.summary)

    private val descriptionCol = new Column[MaintenanceRecord]("Description", _.description, descHTML)

    override val columnList = Seq(dateTimeCol, userCol, summaryCol, descriptionCol)
}
