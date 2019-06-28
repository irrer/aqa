package org.aqa.web

import org.aqa.db.User
import org.aqa.db.Institution
import org.restlet.Response
import org.aqa.db.User.UserInstitution
import org.aqa.web.WebUtil._
import org.aqa.AnonymizeUtil

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

  override def getData(valueMap: ValueMapT, response: Response) = User.listWithDependencies

  override def getPK(value: UserInstitution): Long = value.user.userPK.get
}
