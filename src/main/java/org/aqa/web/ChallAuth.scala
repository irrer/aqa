package org.aqa.web

import org.restlet.Context
import org.restlet.data.ChallengeScheme
import org.restlet.security.ChallengeAuthenticator
import org.aqa.db.UserRole
import org.restlet.Request
import org.restlet.Response
import org.aqa.db.User
import org.restlet.routing.Filter
import org.restlet.data.ChallengeRequest
import org.restlet.data.Status
import org.aqa.db.CachedUser

/**
 * Handle authentication and authorization.  In general, allow open access to public content, but
 * require authentication (login with id and password) for everything else.  Additionally, only let
 * honor requests if the client has the proper authorization level.
 *
 */
class ChallAuth(context: Context, optional: Boolean, challengeScheme: ChallengeScheme, getRequestedRole: (Request, Response) => UserRole.Value)
  extends ChallengeAuthenticator(context, optional, challengeScheme, "Enter your AQA password") {

  private def redirectToLogin(response: Response) = {
    response.redirectSeeOther(Login.path + "?" + Login.messageTag + "=You need to be logged in to perform this action.")
    //response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
    //response.setStatus(Status.SUCCESS_OK)
  }

  private def redirectToBadPassword(response: Response) = {
    response.redirectSeeOther(Login.path + "?" + Login.messageTag + "=Password is incorrect")
  }

  private def redirectToNotAuthorized(response: Response, userRole: UserRole.Value) = {
    (Login.path + "?" + Login.messageTag + "=Access requires " + userRole.toString + " privileges.")
  }

  override def beforeHandle(request: Request, response: Response): Int = {
    println("beforeHandle 1") // TODO rm

    val requestedRole = getRequestedRole(request, response)

    if (requestedRole == UserRole.publik)
      Filter.CONTINUE
    else {

      val chalResp = request.getChallengeResponse

      // defined if user exists
      val user: Option[User] = {
        if (chalResp == null)
          None
        else
          CachedUser.get(chalResp.getIdentifier)
      }

      // true if user exists and is authenticated
      val authenticated: Boolean = {
        user.isDefined &&
          AuthenticationVerifier.validatePassword(new String(chalResp.getSecret), user.get.hashedPassword, user.get.passwordSalt)
      }

      // defined if user exists and is authenticated and has a defined role (should always be defined)
      val userRole: Option[UserRole.Value] = {
        if (user.isDefined && authenticated)
          UserRole.stringToUserRole(user.get.role)
        else
          None
      }

      val authorized: Boolean = userRole.isDefined && (userRole.get.compare(requestedRole) >= 0)

      response.getChallengeRequests.add(new ChallengeRequest(ChallengeScheme.HTTP_BASIC))

      0 match {
        case _ if (!user.isDefined) => { redirectToLogin(response); Filter.STOP }
        case _ if (!authenticated) => { redirectToBadPassword(response); Filter.STOP }
        case _ if (!authorized) => { redirectToNotAuthorized(response, requestedRole); Filter.STOP }
        case _ => Filter.CONTINUE
      }
    }
    //Filter.CONTINUE // set to disable security.  TODO rm
  }

  override def afterHandle(request: Request, response: Response): Unit = {
    println("afterHandle")
    //response.setChallengeRequests(challengeRequests)
  }

  override def createChallengeRequest(stale: Boolean): ChallengeRequest = {
    println("createChallengeRequest")
    super.createChallengeRequest(stale)
  }
  /*     */
}
