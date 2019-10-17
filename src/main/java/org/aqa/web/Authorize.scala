package org.aqa.web

import org.restlet.routing.Filter
import org.restlet.Request
import org.restlet.Response
import org.aqa.db.UserRole
import org.aqa.db.User
import org.restlet.Restlet
import org.restlet.routing.Router
import org.restlet.data.Status
import org.aqa.db.CachedUser

class Authorize(
  publicList: List[Restlet],
  guestList: List[Restlet],
  userList: List[Restlet],
  devList: List[Restlet],
  adminList: List[Restlet],
  router: Router) extends Filter {

  private def getUserRole(id: String): Option[UserRole.Value] = {
    val user = CachedUser.get(id)
    if (user.isDefined) UserRole.stringToUserRole(user.get.role)
    else None
  }

  private def getUserRole(request: Request): Option[UserRole.Value] = {
    val chalResp = request.getChallengeResponse
    if (chalResp == null) None
    else getUserRole(chalResp.getIdentifier)
  }

  private def getRequestedRole(request: Request, response: Response): UserRole.Value = {
    val restlet = router.getNext(request, response)

    0 match {
      case _ if (publicList.contains(restlet)) => UserRole.publik
      case _ if (guestList.contains(restlet)) => UserRole.guest
      case _ if (userList.contains(restlet)) => UserRole.user
      case _ if (devList.contains(restlet)) => UserRole.dev
      case _ => UserRole.admin // default to most restrictive use
    }
  }

  private def notAuthorizedPage(reqRol: UserRole.Value): String = {
    val content = {
      <div>
        You are not authorized to access that page, and must<br/>
        have at least{ reqRol.toString }
        privileges to do so.
      </div>
    }

    WebUtil.wrapBody(content, "Not Authorized")
  }

  private def authorized(request: Request, response: Response): Boolean = {

    val reqRole = getRequestedRole(request, response)

    if (reqRole == UserRole.publik) true
    else {
      val userRole = getUserRole(request)
      val ok = userRole.isDefined && (userRole.get.compare(reqRole) >= 0)
      ok
    }
  }

  private def showUser(request: Request) = { // TODO rm
    val cr = request.getChallengeResponse
    if (cr == null)
      println("User: none")
    else
      println("User: " + cr.getIdentifier)
  }

  override def beforeHandle(request: Request, response: Response): Int = {
    showUser(request)
    if (authorized(request, response))
      Filter.CONTINUE
    else
      Filter.SKIP
  }

  override def afterHandle(request: Request, response: Response) = {
    showUser(request)
    if (!authorized(request, response))
      WebUtil.setResponse(notAuthorizedPage(getRequestedRole(request, response)), response, Status.CLIENT_ERROR_UNAUTHORIZED)
  }
}
