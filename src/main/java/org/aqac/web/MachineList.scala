package org.aqac.web

import org.restlet.Response
import org.aqac.db.Machine
import scala.xml.Elem

object MachineList {
    val path = WebUtil.pathOf(MachineList.getClass.getName)

    type MMI = Machine.MMI

    def redirect(response: Response) = response.redirectSeeOther(path)

    private def machineTypeHTML(mmi: MMI): Elem = <td> { WebUtil.firstPartOf(mmi._2.toName, 40) } </td>

    private def notesHTML(mmi: MMI): Elem = <td> { WebUtil.firstPartOf(mmi._1.notes, 60) } </td>

    private val idCol = new Column[MMI]("Name", _._1.id)

    private val typeCol = new Column[MMI]("URL", _._2.toName, machineTypeHTML)

    private val institutionCol = new Column[MMI]("URL", _._3.name)

    private val notesCol = new Column[MMI]("Notes", _._1.notes, notesHTML)

    val colList = Seq(idCol, typeCol, institutionCol, notesCol)
}

class MachineList extends GenericList[Machine.MMI]("Machine", MachineList.colList) {

    override def getData = Machine.listWithDependencies

    override def getPK(value: Machine.MMI): Long = value._1.machinePK.get
}