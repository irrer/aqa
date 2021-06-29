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

import org.aqa.AnonymizeUtil
import org.aqa.db.CachedUser
import org.aqa.db.User
import org.aqa.db.User.UserInstitution
import org.aqa.web.WebUtil._
import org.restlet.Response

object UserList {
  private val path = new String((new UserList).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)
}

class UserList extends GenericList[UserInstitution] with WebUtil.SubUrlAdmin {

  override val listName = "User"

  private val idCol = new Column[UserInstitution]("Id", _.user.id, ui => makePrimaryKeyHtmlWithAQAAlias(ui.user.id, ui.user.userPK))

  private val nameCol = encryptedColumn("Name", AnonymizeUtil.userAliasFullNamePrefixId, ui => ui.user.userPK.get)

  private val emailCol = encryptedColumn("Email", AnonymizeUtil.userAliasEmailPrefixId, ui => ui.user.userPK.get)

  private val institutionCol = new Column[UserInstitution]("Institution", _.institution.name, ui => wrapAlias(ui.institution.name))

  private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none"

  def getRole(ui: UserInstitution): String = ui.user.role

  private val roleCol = new Column[UserInstitution]("Role", getRole)

  override val columnList = Seq(idCol, nameCol, emailCol, institutionCol, roleCol)

  override def getData(valueMap: ValueMapT, response: Response): Seq[UserInstitution] = {
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
    User.listWithDependencies(instPK)
  }

  override def getPK(value: UserInstitution): Long = value.user.userPK.get

  private def makeButton(buttonType: ButtonType.Value): FormButton = {
    val name = "Refresh"
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show users from all institutions, then click 'Refresh'"), 2, 0)
  val refresh: FormButton = makeButton(ButtonType.BtnPrimary)

  override def htmlFieldList(valueMap: ValueMapT): List[WebRow] = {
    val webRow = new WebRow(List(checkbox, refresh))
    List(webRow)
  }
}
