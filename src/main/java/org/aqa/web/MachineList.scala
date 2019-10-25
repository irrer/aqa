package org.aqa.web

import org.restlet.Response
import org.aqa.db.Machine
import org.aqa.web.WebUtil._
import scala.xml.Elem
import org.aqa.AnonymizeUtil

object MachineList {
  private val path = new String((new MachineList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class MachineList extends GenericList[Machine.MMI] with WebUtil.SubUrlAdmin {

  type MMI = Machine.MMI

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  val checkbox = new WebInputCheckbox("All", true, Some("Show all institutions"), 2, 0)
  val refresh = makeButton("Refresh", ButtonType.BtnPrimary)

  override val filterForm: Option[WebForm] = {
    val form = new WebForm(pathOf, List(List(checkbox, refresh)))
    Some(form)
  }

  override def filterList(mmi: MMI, valueMap: ValueMapT): Boolean = {
    val all = valueMap.get(checkbox.label).isDefined && valueMap(checkbox.label).equalsIgnoreCase("on")
    val ok = all || mmi.institution.institutionPK.get == 1
    ok
  }

  override def listName = "Machine"
  override def getData(valueMap: ValueMapT, response: Response) = Machine.listWithDependencies
  override def getPK(value: Machine.MMI): Long = value.machine.machinePK.get

  private def machineTypeHTML(mmi: MMI): Elem = <div> { WebUtil.firstPartOf(mmi.machineType.toName, 40) } </div>

  private val idCol = new Column[MMI]("Name", _.machine.id, (mmi) => makePrimaryKeyHtmlWithAQAAlias(mmi.machine.id, mmi.machine.machinePK))

  private val typeCol = new Column[MMI]("Machine Type", _.machineType.toName, machineTypeHTML)

  private val mlcCol = new Column[MMI]("MLC", _.mlc.model)

  private val epidCol = new Column[MMI]("EPID", _.epid.model)

  private val serialNoCol = new Column[MMI]("Serial No.", _.machine.serialNumber match { case Some(text) => text; case _ => "not defined" })

  private val institutionCol = new Column[MMI]("Institution", _.institution.name, (mmi) => wrapAlias(mmi.institution.name))

  private def machNotesHtml(mmi: MMI): Elem = {
    wrapAlias(AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, mmi.machine.machinePK.get))
  }

  private val notesCol = new Column[MMI]("Notes", _.machine.id, machNotesHtml _)

  override val columnList = Seq(idCol, institutionCol, typeCol, mlcCol, epidCol, serialNoCol, notesCol)

}