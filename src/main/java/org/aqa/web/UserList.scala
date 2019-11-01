package org.aqa.web

import org.aqa.db.User
import org.aqa.db.Institution
import org.restlet.Response
import org.aqa.db.User.UserInstitution
import org.aqa.web.WebUtil._
import org.aqa.AnonymizeUtil
import org.aqa.db.CachedUser

object UserList {
  private val path = new String((new UserList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class UserList extends GenericList[UserInstitution] with WebUtil.SubUrlAdmin {

  override val listName = "User"

  private val idCol = new Column[UserInstitution]("Id", _.user.id, (ui) => makePrimaryKeyHtmlWithAQAAlias(ui.user.id, ui.user.userPK))

  private val nameCol = encryptedColumn("Name", AnonymizeUtil.userAliasFullNamePrefixId, (ui) => ui.user.userPK.get)

  private val emailCol = encryptedColumn("Email", AnonymizeUtil.userAliasEmailPrefixId, (ui) => ui.user.userPK.get)

  private val institutionCol = new Column[UserInstitution]("Institution", _.institution.name, (ui) => wrapAlias(ui.institution.name))

  private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

  def getRole(ui: UserInstitution): String = ui.user.role

  private val roleCol = new Column[UserInstitution]("Role", getRole)

  override val columnList = Seq(idCol, nameCol, emailCol, institutionCol, roleCol)

  override def getData(valueMap: ValueMapT, response: Response) = {
    val v = valueMap.get(checkbox.label)
    val all = v.isDefined && (v.get.equalsIgnoreCase("true") || v.get.equalsIgnoreCase("on"))
    val instPK = {
      if (all) None
      else {
        val userIdReal = valueMap(userIdRealTag)
        val user = CachedUser.get(valueMap(userIdRealTag)).get
        Some(user.institutionPK)
      }
    }
    User.listWithDependencies(instPK)
  }

  override def getPK(value: UserInstitution): Long = value.user.userPK.get

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show users from all institutions, then click 'Refresh'"), 2, 0)
  val refresh = makeButton("Refresh", ButtonType.BtnPrimary)

  override def htmlFieldList(valueMap: ValueMapT): List[WebRow] = {
    val webRow = new WebRow(List(checkbox, refresh))
    List(webRow);
  }
}
