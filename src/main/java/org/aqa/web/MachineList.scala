/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show machines from all institutions, then click 'Refresh'"), 2, 0)
  val refresh = makeButton("Refresh", ButtonType.BtnPrimary)

  override def htmlFieldList(valueMap: ValueMapT): List[WebRow] = {
    val webRow = new WebRow(List(checkbox, refresh))
    List(webRow);
  }

  override def listName = "Machine"

  override def getData(valueMap: ValueMapT, response: Response) = {

    val v = valueMap.get(checkbox.label)
    val all = v.isDefined && (v.get.equalsIgnoreCase("true") || v.get.equalsIgnoreCase("on"))
    val instPK = {
      if (all || userIsWhitelisted(response)) None
      else {
        val userIdReal = valueMap(userIdRealTag)
        val user = CachedUser.get(valueMap(userIdRealTag)).get
        Some(user.institutionPK)
      }
    }
    Machine.listWithDependencies(instPK)
  }
  
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