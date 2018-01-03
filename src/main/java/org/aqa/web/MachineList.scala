package org.aqa.web

import org.restlet.Response
import org.aqa.db.Machine
import org.aqa.web.WebUtil._
import scala.xml.Elem

object MachineList {
  private val path = new String((new MachineList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class MachineList extends GenericList[Machine.MMI] with WebUtil.SubUrlAdmin {

  type MMI = Machine.MMI

  override def listName = "Machine"
  override def getData(valueMap: ValueMapT) = Machine.listWithDependencies
  override def getPK(value: Machine.MMI): Long = value.machine.machinePK.get

  private def machineTypeHTML(mmi: MMI): Elem = <div> { WebUtil.firstPartOf(mmi.machineType.toName, 40) } </div>

  private def notesHTML(mmi: MMI): Elem = <div> { WebUtil.firstPartOf(mmi.machine.notes, 60) } </div>

  private val idCol = new Column[MMI]("Name", _.machine.id, (mmi) => makePrimaryKeyHtml(mmi.machine.id, mmi.machine.machinePK))

  private val typeCol = new Column[MMI]("Machine Type", _.machineType.toName, machineTypeHTML)

  private val mlcCol = new Column[MMI]("MLC", _.mlc.model)

  private val epidCol = new Column[MMI]("EPID", _.epid.model)

  private val serialNoCol = new Column[MMI]("Serial No.", _.machine.serialNumber match { case Some(text) => text; case _ => "not defined" })

  private val institutionCol = new Column[MMI]("Institution", _.institution.name)

  private val notesCol = new Column[MMI]("Notes", _.machine.notes, notesHTML)

  override val columnList = Seq(idCol, institutionCol, typeCol, mlcCol, epidCol, serialNoCol, notesCol)
}