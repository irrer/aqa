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
import org.aqa.web.WebUtil._
import org.aqa.Config
import java.io.File
import org.aqa.db.SystemModification

object SystemModificationList {
  val path = new String((new SystemModificationList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)

}

class SystemModificationList extends GenericList[SystemModification.SystemModificationComposite] with WebUtil.SubUrlAdmin {

  private type SMC = SystemModification.SystemModificationComposite

  override def listName = "System Modification"

  private def compareByDate(a: SMC, b: SMC): Boolean = a.systemModification.date.compareTo(b.systemModification.date) > 0

  private def dateToString(smc: SMC): String = SystemModificationUpdate.dateFormat.format(smc.systemModification.date)

  private val dateCol = new Column[SMC]("Date", compareByDate _, (smc) => makePrimaryKeyHtml(dateToString(smc), smc.systemModification.systemModificationPK))

  //private val userCol = new Column[SMC]("User", _.user.id, (smc) => wrapAlias(smc.user.id))

  private val summaryCol = new Column[SMC](
    "Summary", _.systemModification.summary, (smc) => <div>{ WebUtil.firstPartOf(smc.systemModification.summary, 120) }</div>)

  private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

  //override val columnList = Seq(dateCol, userCol, summaryCol)
  override val columnList = Seq(dateCol, summaryCol)

  override def getData(valueMap: ValueMapT, response: Response) = SystemModification.listWithDependencies.sortBy(_.systemModification.date.getTime)

  override def getPK(smc: SMC): Long = smc.systemModification.systemModificationPK.get
}
