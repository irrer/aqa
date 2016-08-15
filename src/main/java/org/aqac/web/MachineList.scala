package org.aqac.web

import org.restlet.Response
import org.aqac.db.Machine
import org.aqac.web.WebUtil._
import scala.xml.Elem

object MachineList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MachineList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class MachineList extends GenericList[Machine.MMI] with WebUtil.SubUrlAdmin {

    type MMI = Machine.MMI

    override def listName = "Machine"
    override def getData = Machine.listWithDependencies
    override def getPK(value: Machine.MMI): Long = value.machine.machinePK.get

    private def machineTypeHTML(mmi: MMI): Elem = <div> { WebUtil.firstPartOf(mmi.machineType.toName, 40) } </div>

    private def notesHTML(mmi: MMI): Elem = <div> { WebUtil.firstPartOf(mmi.machine.notes, 60) } </div>

    private val idCol = new Column[MMI]("Name", _.machine.id, (mmi) => makePrimaryKeyHtml(mmi.machine.id, mmi.machine.machinePK))

    private val typeCol = new Column[MMI]("Machine Type", _.machineType.toName, machineTypeHTML)

    private val institutionCol = new Column[MMI]("Institution", _.institution.name)

    private val notesCol = new Column[MMI]("Notes", _.machine.notes, notesHTML)

    override val columnList = Seq(idCol, typeCol, institutionCol, notesCol)
}