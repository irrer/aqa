package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.MachineType

object MachineTypeList {
    val path = WebUtil.pathOf(MachineTypeList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)

    private def notesHTML(machineType: MachineType): Elem = <td> { WebUtil.firstPartOf(machineType.notes, 60) } </td>

    private val manufacturerCol = new Column[MachineType]("Manufacturer", _.manufacturer)

    private val modelCol = new Column[MachineType]("Model", _.model)

    private val versionCol = new Column[MachineType]("Version", _.version)

    private val notesCol = new Column[MachineType]("Notes", _.notes, notesHTML)

    val colList = Seq(manufacturerCol, modelCol, versionCol, notesCol)

}

class MachineTypeList extends GenericList[MachineType]("MachineType", MachineTypeList.colList) {

    override def getData = MachineType.list

    override def getPK(value: MachineType): Long = value.machineTypePK.get
}
