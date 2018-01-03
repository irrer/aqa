package org.aqa.web

import org.aqa.db.User
import org.aqa.db.Institution
import org.restlet.Response
import org.aqa.db.User.UserInstitution
import org.aqa.web.WebUtil._

object UserList {
  private val path = new String((new UserList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class UserList extends GenericList[UserInstitution] with WebUtil.SubUrlAdmin {

  override val listName = "User"

  private val idCol = new Column[UserInstitution]("Name", _.user.id, (ui) => makePrimaryKeyHtml(ui.user.id, ui.user.userPK))

  private val nameCol = new Column[UserInstitution]("Name", _.user.fullName)

  private val emailCol = new Column[UserInstitution]("Email", _.user.email)

  private val institutionCol = new Column[UserInstitution]("Institution", _.institution.name)

  private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

  def getRole(ui: UserInstitution): String = ui.user.role

  private val roleCol = new Column[UserInstitution]("Role", getRole)

  override val columnList = Seq(idCol, nameCol, emailCol, institutionCol, roleCol)

  override def getData(valueMap: ValueMapT) = User.listWithDependencies

  override def getPK(value: UserInstitution): Long = value.user.userPK.get
}
