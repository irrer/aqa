package org.aqac.web

import org.aqac.db.User
import org.aqac.db.Institution
import org.restlet.Response
import org.aqac.db.User.UserInstitution

object UserList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, UserList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class UserList extends GenericList[UserInstitution] with WebUtil.SubUrlAdmin {

    override val listName = "User"

    private val idCox = new Column[UserInstitution]("Id", _.user.id)
    private val idCol = new Column[UserInstitution]("Name", _.user.id, (ui) => makePrimaryKeyHtml(ui.user.id, ui.user.userPK))

    private val nameCol = new Column[UserInstitution]("Name", _.user.fullName)

    private val emailCol = new Column[UserInstitution]("Email", _.user.email)

    private val institutionCol = new Column[UserInstitution]("Institution", _.institution.name)

    private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

    def getRole(ui: UserInstitution): String = ui.user.role

    private val roleCol = new Column[UserInstitution]("Role", getRole)

    override val columnList = Seq(idCol, nameCol, emailCol, institutionCol, roleCol)

    override def getData = User.listWithDependencies

    override def getPK(value: UserInstitution): Long = value.user.userPK.get
}
