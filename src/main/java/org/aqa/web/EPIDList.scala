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
import org.aqa.db.EPID
import org.aqa.web.WebUtil._

object EPIDList {
  private val path = new String((new EPIDList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class EPIDList extends GenericList[EPID] with WebUtil.SubUrlAdmin {
  override def listName = "EPID"

  override def getPKName = "epidPK"

  override def getData(valueMap: ValueMapT, response: Response) = EPID.list

  override def getPK(value: EPID): Long = value.epidPK.get

  private def notesHTML(machineType: EPID): Elem = <div>{ WebUtil.firstPartOf(machineType.notes, 60) }</div>

  private val manufacturerCol = new Column[EPID]("Manufacturer", _.manufacturer, (mlc) => makePrimaryKeyHtml(mlc.manufacturer, mlc.epidPK))

  private val modelCol = new Column[EPID]("Model", _.model)

  private val versionCol = new Column[EPID]("HW Version", _.hardwareVersion)

  private val notesCol = new Column[EPID]("Notes", _.notes, notesHTML)

  override val columnList = Seq(manufacturerCol, modelCol, versionCol, notesCol)
}
