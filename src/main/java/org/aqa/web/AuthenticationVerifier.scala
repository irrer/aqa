package org.aqa.web

import org.restlet.security.Verifier
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import org.restlet.data.MediaType
import org.aqa.Config
import org.aqa.db.User
import org.aqa.Util
import org.aqa.db.DbSetup
import org.aqa.Logging
import org.restlet.engine.header.ChallengeWriter
import org.aqa.db.UserRole
import org.aqa.db.CachedUser
import org.aqa.Crypto
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.Level2Ldap
import org.aqa.db.Institution
import org.aqa.AnonymizeUtil

class AuthenticationVerifier(getRequestedRole: (Request, Response) => UserRole.Value) extends Verifier with Logging {

  override def verify(request: Request, response: Response): Int = {
    val requestedRole = getRequestedRole(request, response)
    if (requestedRole.id == UserRole.publik.id) Verifier.RESULT_VALID // let anyone into public areas
    else {
      request.getChallengeResponse match {
        case null => Verifier.RESULT_MISSING // The CachedUser.get function checks for this, but checking it here allows for finer grained reporting of the credentials.
        case challResp => {
          val user = CachedUser.get(request)

          val status: Int = {
            CachedUser.get(request) match {
              case Some(user) => Verifier.RESULT_VALID
              case _ => Verifier.RESULT_UNKNOWN
            }
          }
          status
        }
      }
    }

    //Verifier.RESULT_VALID  // uncomment this line for testing to allow users to use the system without authenticating
  }
}

object AuthenticationVerifier {

  private val minPasswordSize = 8

  private def containsNonAlpha(passwordText: String): Boolean = passwordText.toLowerCase.replaceAll("[a-z]", "").size > 0

  /**
   * Only permit high quality passwords.
   */
  def judgePassword(password: String): Option[String] = {
    0 match {
      case _ if (password.size < minPasswordSize) => Some("Password must be at least " + minPasswordSize + " characters long.")
      case _ if (!containsNonAlpha(password)) => Some("Password must contain at least one non-alpha character.")
      case _ => None // good enough
    }
  }

  def verifierResultToString(result: Int): Option[String] = {
    result match {
      case Verifier.RESULT_INVALID => Some("RESULT_INVALID")
      case Verifier.RESULT_MISSING => Some("RESULT_MISSING")
      case Verifier.RESULT_STALE => Some("RESULT_STALE")
      case Verifier.RESULT_UNSUPPORTED => Some("RESULT_UNSUPPORTED")
      case Verifier.RESULT_UNKNOWN => Some("RESULT_UNKNOWN")
      case Verifier.RESULT_VALID => Some("RESULT_VALID")
      case _ => None
    }
  }

}
