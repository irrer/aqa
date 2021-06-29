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
import scala.xml.Elem
import org.aqa.db.MaintenanceRecord
import java.sql.Timestamp
import org.aqa.web.WebUtil._
import org.aqa.db.User
import org.aqa.db.Machine

object MaintenanceRecordList {
  val path = new String((new MaintenanceRecordList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class MaintenanceRecordList extends GenericList[MaintenanceRecord] with WebUtil.SubUrlAdmin {
  override def listName = "MaintenanceRecord"

  override def getPKName: String = "maintenanceRecordPK"

  /**
   * If a machinePK is given, then filter on that, otherwise list maintenance records for all machines.
   */
  override def getData(valueMap: ValueMapT, response: Response) = {
    valueMap.get(MachineUpdate.machinePKTag) match {
      case Some(machinePK) => MaintenanceRecord.getByMachine(machinePK.toLong)
      case _ => MaintenanceRecord.list
    }
  }

  override def getPK(value: MaintenanceRecord): Long = value.maintenanceRecordPK.get

  private def machineParameter(valueMap: ValueMapT): String = { "?" + MachineUpdate.machinePKTag + "=" + valueMap(MachineUpdate.machinePKTag) }

  override def createNewPath(valueMap: ValueMapT): String = {
    WebUtil.cleanClassName(MaintenanceRecordUpdate.getClass.getName) + machineParameter(valueMap)
  }

  override def createNew(valueMap: ValueMapT): Elem = {
    val machinePath = WebUtil.cleanClassName(MachineUpdate.getClass.getName) + machineParameter(valueMap)
    val machineName: String = {
      try {
        Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get.id
      } catch {
        case t: Throwable => ""
      }
    }
    <div class="row col-md-2 col-md-offset-10">
      <strong>
        <a href={ createNewPath(valueMap) }>Create new { listName }</a><p> </p>
        <a href={ machinePath }>Return to machine { " " + machineName }</a><p> </p>
      </strong>
    </div>;
  }

  private def descHTML(maintenanceRecord: MaintenanceRecord): Elem = <div>{ WebUtil.firstPartOf(maintenanceRecord.description, 60) }</div>

  private val dateTimeCol = new Column[MaintenanceRecord](
    "Date/Time",
    (a, b) => (a.creationTime.getTime < b.creationTime.getTime), (mr: MaintenanceRecord) => makePrimaryKeyHtml(WebInputDateTime.dateTimeFormat.format(mr.creationTime), mr.maintenanceRecordPK))

  private val userCol = new Column[MaintenanceRecord]("User", mr => User.get(mr.userPK).get.id, mr => wrapAlias(User.get(mr.userPK).get.id))

  private val summaryCol = new Column[MaintenanceRecord]("Summary", _.summary)

  private val descriptionCol = new Column[MaintenanceRecord]("Description", _.description, descHTML)

  override val columnList = Seq(dateTimeCol, userCol, summaryCol, descriptionCol)
}
