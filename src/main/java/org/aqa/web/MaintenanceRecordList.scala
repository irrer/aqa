package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.MaintenanceRecord
import java.sql.Timestamp

object MaintenanceRecordList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MaintenanceRecordList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class MaintenanceRecordList extends GenericList[MaintenanceRecord] with WebUtil.SubUrlAdmin {
    override def listName = "MaintenanceRecord"

    override def getData = MaintenanceRecord.list // TODO must specify machinePK

    override def getPK(value: MaintenanceRecord): Long = value.maintenanceRecordPK.get

    private def descHTML(maintenanceRecord: MaintenanceRecord): Elem = <div>{ WebUtil.firstPartOf(maintenanceRecord.description, 60) }</div>

    private val dateTimeCol = new Column[MaintenanceRecord]("Date/Time", _.dateTime.toString)

    private val userCol = new Column[MaintenanceRecord]("User", (a, b) => (a.dateTime.getTime < b.dateTime.getTime), (user: MaintenanceRecord) => makePrimaryKeyHtml("user", Some(user.userPK))) //(user:MaintenanceRecord) => makePrimaryKeyHtml("user", Some(user.userPK)))

    private val maintenanceTypeCol = new Column[MaintenanceRecord]("Type", _.maintenanceType)

    private val descriptionCol = new Column[MaintenanceRecord]("Description", _.description, descHTML)

    override val columnList = Seq(dateTimeCol, userCol, maintenanceTypeCol, descriptionCol)
}
