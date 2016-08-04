package org.aqac.web

import org.aqac.db.User
import org.aqac.db.Institution
import org.restlet.Response

object UserList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, UserList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)

    private val idCol = new Column[(User, Institution)]("Id", _._1.id)

    private val nameCol = new Column[(User, Institution)]("Name", _._1.fullName)

    private val emailCol = new Column[(User, Institution)]("Email", _._1.email)

    private val institutionCol = new Column[(User, Institution)]("Institution", _._2.name)

    private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

    def getRole(ui: (User, Institution)): String = ui._1.role

    private val roleCol = new Column[(User, Institution)]("Role", getRole)

    val colList = Seq(idCol, nameCol, emailCol, institutionCol, roleCol)
}

class UserList extends GenericList[(User, Institution)]("User", UserList.colList) with WebUtil.SubUrlAdmin {

    override def getData = User.listWithDependencies

    override def getPK(value: (User, Institution)): Long = value._1.userPK.get

}
