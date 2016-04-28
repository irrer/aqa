package org.aqac.web

import org.aqac.db.User
import org.aqac.db.Institution
import org.restlet.Response

object UserList {
    val path = "/UserList"

    def redirect(response: Response) = response.redirectSeeOther(path)

    private val nameCol = new Column[(User, Institution)]("Name", _._1.fullName)

    private val emailCol = new Column[(User, Institution)]("Email", _._1.email)

    private val institutionCol = new Column[(User, Institution)]("Institution", _._2.name)

    val colList = Seq(nameCol, emailCol, institutionCol)
}

class UserList extends GenericList[(User, Institution)]("User", UserList.colList) {

    override def getData = User.listWithDependencies

    override def getPK(value: (User, Institution)): Long = value._1.userPK.get

}
