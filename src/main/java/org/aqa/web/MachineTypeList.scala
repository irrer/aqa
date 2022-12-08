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
import org.aqa.db.MachineType
import org.aqa.web.WebUtil._

object MachineTypeList {
  private val path = new String((new MachineTypeList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class MachineTypeList extends GenericList[MachineType] with WebUtil.SubUrlAdmin {
  override def listName = "MachineType"

  override def getData(valueMap: ValueMapT, response: Response) = MachineType.list

  override def getPK(value: MachineType): Long = value.machineTypePK.get

  private def notesHTML(machineType: MachineType): Elem = <div>{ WebUtil.firstPartOf(machineType.notes, 60) }</div>

  private val modelCol = new Column[MachineType]("Model", _.model, (mt) => makePrimaryKeyHtml(mt.model, mt.machineTypePK))

  private val manufacturerCol = new Column[MachineType]("Manufacturer", _.manufacturer)

  private val versionCol = new Column[MachineType]("Version", _.version)

  private val notesCol = new Column[MachineType]("Notes", _.notes, notesHTML)

  override val columnList = Seq(modelCol, manufacturerCol, versionCol, notesCol)
}
