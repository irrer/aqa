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
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._

object ProcedureList {
  private val path = new String((new ProcedureList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class ProcedureList extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlAdmin {

  override val listName = "Procedure"

  type PU = Procedure.ProcedureUser

  private val idCol = new Column[PU]("Name", _.procedure.name, (inst) => makePrimaryKeyHtml(inst.procedure.name, inst.procedure.procedurePK))

  private def userToHtml(pu: PU) = <span aqaalias="">{ pu.user.id }</span>
  private val supportedByCol = new Column[PU]("Supported By", _.user.fullName_real, userToHtml)

  private val versionCol = new Column[PU]("Version", _.procedure.version)

  private def notesHTML(pu: PU): Elem = { <div>{ WebUtil.firstPartOf(pu.procedure.notes, 60) }</div> }

  private val notesCol = new Column[PU]("Notes", _.procedure.notes, notesHTML)

  override val columnList = Seq(idCol, supportedByCol, versionCol, notesCol)

  override def getData(valueMap: ValueMapT, response: Response) = Procedure.listWithDependencies

  override def getPK(value: PU): Long = value.procedure.procedurePK.get
}
