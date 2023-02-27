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

import org.aqa.db.CachedUser
import org.aqa.db.MachineWL
import org.aqa.web.WebUtil._
import org.restlet.Response

import scala.xml.Elem

object MachineWLList {
  private val path = new String((new MachineWLList).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)
}

class MachineWLList extends GenericList[MachineWL.MachineWLExtended] with WebUtil.SubUrlAdmin {

  private type MQL = MachineWL.MachineWLExtended
  override val canCreate: Boolean = false // do not allow user to create, they can only edit
  override def listName = "Winston Lutz Parameters by Machine"

  override def getData(valueMap: ValueMapT, response: Response): Seq[MachineWL.MachineWLExtended] = {

    val instPK = {
      val user = CachedUser.get(valueMap(userIdRealTag)).get
      user.institutionPK
    }
    MachineWL.listExtended(instPK)
  }

  override def getPK(mdq: MQL): Long = mdq.machine.machinePK.get

  override def getPKName: String = MachineUpdate.machinePKTag

  override def updatePath: String = SubUrl.url(subUrl, "MachineWLUpdate")

  private def machineTypeHTML(mdq: MQL): Elem = <div> {WebUtil.firstPartOf(mdq.machineType.toName, 40)} </div>

  private val idCol = new Column[MQL]("Name", _.machine.id, mmi => makePrimaryKeyHtmlWithAQAAlias(mmi.machine.id, mmi.machine.machinePK))

  private val typeCol = new Column[MQL]("Machine Type", _.machineType.toName, machineTypeHTML)

  private val passCol = new Column[MQL]("Pass (mm)", _.machineWL.passLimit_mm.toString)

  private val ballDiameterCol = new Column[MQL]("Ball Diameter (mm)", _.machineWL.ballDiameter_mm.toString)

  override val columnList: Seq[Column[MQL]] = Seq(idCol, typeCol, passCol, ballDiameterCol)

}
