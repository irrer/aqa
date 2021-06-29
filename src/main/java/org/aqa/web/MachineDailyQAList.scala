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
import org.aqa.db.MachineDailyQA
import org.aqa.web.WebUtil._
import org.restlet.Response

import scala.xml.Elem

object MachineDailyQAList {
  private val path = new String((new MachineDailyQAList).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)
}

class MachineDailyQAList extends GenericList[MachineDailyQA.MachineDailyQAExtended] with WebUtil.SubUrlAdmin {

  type MDQ = MachineDailyQA.MachineDailyQAExtended

  override def listName = "Daily QA Parameter"

  override def getData(valueMap: ValueMapT, response: Response): Seq[MachineDailyQA.MachineDailyQAExtended] = {

    val instPK = {
      val user = CachedUser.get(valueMap(userIdRealTag)).get
      user.institutionPK
    }
    MachineDailyQA.listExtended(instPK)
  }

  override def getPK(mdq: MDQ): Long = mdq.machine.machinePK.get

  override def getPKName: String = MachineUpdate.machinePKTag

  override def updatePath: String = SubUrl.url(subUrl, "MachineDailyQAUpdate")

  private def machineTypeHTML(mdq: MDQ): Elem = <div> {WebUtil.firstPartOf(mdq.machineType.toName, 40)} </div>

  private val idCol = new Column[MDQ]("Name", _.machine.id, mmi => makePrimaryKeyHtmlWithAQAAlias(mmi.machine.id, mmi.machine.machinePK))

  private val typeCol = new Column[MDQ]("Machine Type", _.machineType.toName, machineTypeHTML)

  private val passCol = new Column[MDQ]("Pass (mm)", _.machineDailyQA.passLimit_mm.toString)

  private val warningCol = new Column[MDQ]("Warning (mm)", _.machineDailyQA.warningLimit_mm.toString)

  private val requireXRayOffsetCol = new Column[MDQ]("Require X-Ray Offset", _.machineDailyQA.requireXRayOffset.toString)

  override val columnList = Seq(idCol, typeCol, passCol, warningCol, requireXRayOffsetCol)

}
