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

import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.restlet.Response

import scala.xml.Elem

// TODO can this file be removed?

object XRunProcedureList {
  private val path = new String((new XRunProcedureList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class XRunProcedureList extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlRun {

  type PU = Procedure.ProcedureUser
  override def listName = "Procedure"

  private def notesHTML(pu: PU): Elem = <div>{WebUtil.firstPartOf(pu.procedure.notes, 60)}</div>

  private val idCol = new Column[PU]("Name", _.procedure.name, (pu) => makePrimaryKeyHtml(pu.procedure.name, pu.procedure.procedurePK))

  private val versionCol = new Column[PU]("Version", _.procedure.version)

  private val notesCol = new Column[PU]("Notes", _.procedure.notes, notesHTML)

  override val columnList = Seq(idCol, versionCol, notesCol)

  override def getData(valueMap: ValueMapT, response: Response) = Procedure.listWithDependencies

  override def getPK(value: PU): Long = value.procedure.procedurePK.get
}
