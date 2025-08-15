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

import scala.xml.Elem

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

  private def makeApproverCheckbox(userInstitution: UserInstitution): Elem = {
    val id = "approver_" + userInstitution.user.userPK.get
    val checkboxValue = userInstitution.user.isApprover
    val value = if (checkboxValue) "true" else ""

    val cb1 = {
        <input id={id} prevstate={value} type="checkbox" class="form-control" onclick={s"SetApproverState(${userInstitution.user.userPK.get})"} />
    }
    if (checkboxValue) {
      val cb2 = WebUtil.addAttr(cb1, "checked", "true")
      val cb3 = WebUtil.addAttr(cb2, "value", "")
      cb3
    }
    else
      cb1
  }

  private val approverCol = new Column[UserInstitution]("Approver", (a, b) => a.user.role.compareTo(b.user.role) > 0, ui => makeApproverCheckbox(ui) )

  private val roleCol = new Column[UserInstitution]("Role", _.user.role)

  override val columnList = Seq(idCol, nameCol, emailCol, institutionCol, approverCol, roleCol)

  override def getData(valueMap: ValueMapT, response: Response): Seq[UserInstitution] = {
    val v = valueMap.get(checkbox.label)
    val all = v.isDefined && (v.get.equalsIgnoreCase("true") || v.get.equalsIgnoreCase("on"))
    val instPK = {
      if (all || userIsWhitelisted(response)) None
      else {
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

  override def makeRunScript(): Option[String] = {
    Some(s"""
       |
       |function SetApproverState(userPK) {
       |
       |  var xhttp = new XMLHttpRequest();
       |
       |  var cbId = "approver_" + userPK;  // checkbox id
       |
       |  var cb = document.getElementById(cbId);  // checkbox
       |  var cbPrevState = cb.getAttribute("prevstate");
       |  // var cbState = cb.value;
       |  var cbChecked = cb.checked;
       |
       |  xhttp.onreadystatechange = function() {
       |    if (this.readyState == 4) {
       |      if (this.status == 200) {
       |        var newState = this.responseText;
       |        cb.setAttribute("prevstate", newState);
       |        var can = (newState === "true");
       |        cb.checked = (newState === "true");
       |      }
       |      else {
       |        cb.checked = (cbPrevState === "true");
       |      }
       |    }
       |  };
       |
       |
       |  xhttp.open("POST", "/SetApproverStateRestlet?state=" + cbChecked + "&userPK=" + userPK, true);
       |  xhttp.send();
       |
       |}
       |""".stripMargin)
  }
}
