package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.MachineType

object MachineTypeList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MachineTypeList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class MachineTypeList extends GenericList[MachineType] with WebUtil.SubUrlAdmin {
    override def listName = "MachineType"

    override def getData = MachineType.list

    override def getPK(value: MachineType): Long = value.machineTypePK.get

    private def notesHTML(machineType: MachineType): Elem = <div>{ WebUtil.firstPartOf(machineType.notes, 60) }</div>

    private val manufacturerCol = new Column[MachineType]("Manufacturer", _.manufacturer, (mt) => makePrimaryKeyHtml(mt.manufacturer, mt.machineTypePK))

    private val modelCol = new Column[MachineType]("Model", _.model)

    private val versionCol = new Column[MachineType]("Version", _.version)

    private val notesCol = new Column[MachineType]("Notes", _.notes, notesHTML)

    override val columnList = Seq(manufacturerCol, modelCol, versionCol, notesCol)

}
