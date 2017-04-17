package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.MachineType
import org.aqa.web.WebUtil._

object MachineTypeList {
    private val path = new String((new MachineTypeList).pathOf)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class MachineTypeList extends GenericList[MachineType] with WebUtil.SubUrlAdmin {
    override def listName = "MachineType"

    override def getData(valueMap: ValueMapT) = MachineType.list

    override def getPK(value: MachineType): Long = value.machineTypePK.get

    private def notesHTML(machineType: MachineType): Elem = <div>{ WebUtil.firstPartOf(machineType.notes, 60) }</div>

    private val modelCol = new Column[MachineType]("Model", _.model, (mt) => makePrimaryKeyHtml(mt.model, mt.machineTypePK))

    private val manufacturerCol = new Column[MachineType]("Manufacturer", _.manufacturer)

    private val versionCol = new Column[MachineType]("Version", _.version)

    private val notesCol = new Column[MachineType]("Notes", _.notes, notesHTML)

    override val columnList = Seq(modelCol, manufacturerCol, versionCol, notesCol)
}
