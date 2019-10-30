package org.aqa.web

import org.restlet.Response
import org.aqa.db.Machine
import org.aqa.web.WebUtil._
import scala.xml.Elem
import org.aqa.AnonymizeUtil
import org.aqa.db.CachedUser

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

  val checkbox = new WebInputCheckbox("All", true, Some("Show machines from all institutions"), 2, 0)
  val refresh = makeButton("Refresh", ButtonType.BtnPrimary)

  //  override def filterForm(valueMap: ValueMapT): Option[WebForm] = {
  //    val form = new WebForm(pathOf, List(List(checkbox, refresh)))
  //    Some(form)
  //  }
  //
  //  override def filterList(mmi: MMI, valueMap: ValueMapT): Boolean = {
  //    val all = valueMap.get(checkbox.label).isDefined && valueMap(checkbox.label).equalsIgnoreCase("on")
  //    val ok = all || mmi.institution.institutionPK.get == CachedUser.get(valueMap(userIdRealTag)).get.institutionPK
  //    ok
  //  }

  override def filterList(mmi: MMI, valueMap: ValueMapT): Boolean = {
    try {
      val userIdReal = valueMap(userIdRealTag)
      val user = CachedUser.get(valueMap(userIdRealTag)).get
      val ok = userIsWhitelisted(userIdReal) || (mmi.institution.institutionPK.get == user.institutionPK)
      ok
    } catch {
      case t: Throwable => {
        logger.warn("Error filtering machine list: " + fmtEx(t))
        true
      }
    }
  }

  override def listName = "Machine"
  override def getData(valueMap: ValueMapT, response: Response) = Machine.listWithDependencies
  override def getPK(value: Machine.MMI): Long = value.machine.machinePK.get

  private def machineTypeHTML(mmi: MMI): Elem = <div> { WebUtil.firstPartOf(mmi.machineType.toName, 40) } </div>

  private val idCol = new Column[MMI]("Name", _.machine.id, (mmi) => makePrimaryKeyHtmlWithAQAAlias(mmi.machine.id, mmi.machine.machinePK))

  private val typeCol = new Column[MMI]("Machine Type", _.machineType.toName, machineTypeHTML)

  private val mlcCol = new Column[MMI]("MLC", _.mlc.model)

  private val epidCol = new Column[MMI]("EPID", _.epid.model)

  private def compareDevSerNo(mmiA: MMI, mmiB: MMI): Boolean = {
    val a = mmiA.machine.serialNumber
    val b = mmiB.machine.serialNumber
    val cmpr = (a.isDefined, b.isDefined) match {
      case (true, true) => a.get.compareTo(b.get) < 0
      case (false, false) => false
      case (true, false) => false
      case (false, true) => true
    }
    cmpr
  }

  private def devSerNo(mmi: MMI) = mmi.machine.serialNumber match { case Some(text) => wrapAlias(text); case _ => (<div>not defined</div>) }

  private val serialNoCol = new Column[MMI]("Serial No.", compareDevSerNo _, devSerNo _)

  private val institutionCol = new Column[MMI]("Institution", _.institution.name, (mmi) => wrapAlias(mmi.institution.name))

  private def machNotesHtml(mmi: MMI): Elem = {
    wrapAlias(AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, mmi.machine.machinePK.get))
  }

  private val notesCol = new Column[MMI]("Notes", _.machine.id, machNotesHtml _)

  override val columnList = Seq(idCol, institutionCol, typeCol, mlcCol, epidCol, serialNoCol, notesCol)

}