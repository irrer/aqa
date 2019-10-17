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

class AuthenticationVerifier(getRequestedRole: (Request, Response) => UserRole.Value) extends Verifier with Logging {

  /**
   * Check to see if the credentials are valid, first using the cache for efficiency and
   * then if necessary the database.
   *
   * Note that an invalid result from the cache may indicate that the password has been
   * recently changed, so it is appropriate to check the database.
   *
   * The usual case will be that the user enters a valid password, so execution is optimized
   * for that.
   */
  private def check(id: String, secret: String): Int = {
    CachedUser.get(id) match {
      case Some(user) if AuthenticationVerifier.validatePassword(secret, user.hashedPassword, user.passwordSalt) => Verifier.RESULT_VALID
      case Some(user) => {
        logger.warn("the password of user " + id + " failed to authenticate")
        Verifier.RESULT_INVALID
      }
      case _ => Verifier.RESULT_UNKNOWN
    }
  }

  override def verify(request: Request, response: Response): Int = {
    if (true) { // TODO rm
      val cr = request.getChallengeResponse
      if (cr == null)
        Trace.trace("no user")
      else {
        val id = cr.getIdentifier
        val pw = cr.getSecret.mkString("")
        Trace.trace("User: " + id + "  password: " + pw)
      }
    }
    val requestedRole = getRequestedRole(request, response)
    if (requestedRole.id == UserRole.publik.id) Verifier.RESULT_VALID // let anyone into public areas
    else {
      request.getChallengeResponse match {
        case null => Verifier.RESULT_MISSING
        case challResp => {
          val id = challResp.getIdentifier
          val secret = new String(challResp.getSecret)

          CachedUser.get(id) match {
            case Some(user) if AuthenticationVerifier.validatePassword(secret, user.hashedPassword, user.passwordSalt) => Verifier.RESULT_VALID
            case Some(user) => {
              logger.warn("Authentication violation.  The password of user " + id + " failed")
              Verifier.RESULT_INVALID
            }
            case _ => {
              logger.warn("unknown user " + id + " failed")
              Verifier.RESULT_UNKNOWN
            }
          }
        }
      }
    }

    //Verifier.RESULT_VALID  // uncomment this line for testing to allow users to use the system without authenticating
  }
}

object AuthenticationVerifier {

  //    class CachedUser(val user: User) {
  //        val timeout = System.currentTimeMillis + Config.AuthenticationTimeoutInMs
  //    }
  //
  //    private val cache = scala.collection.mutable.HashMap[String, CachedUser]()
  //
  //    def put(id: String, secret: String, user: User): Unit = {
  //        cache.synchronized({
  //            cache.put(id, new CachedUser(user))
  //        })
  //    }
  //
  //    def getFromCache(id: String): Option[CachedUser] = {
  //        cache.synchronized({
  //            cache.get(id)
  //        })
  //    }
  //
  //    def clean: Unit = {
  //        cache.synchronized({
  //            val now = System.currentTimeMillis
  //            val expired = cache.filter(c => c._2.timeout < now).map(c1 => c1._1)
  //            cache --= expired
  //        })
  //    }
  //
  //    def remove(id: String): Option[CachedUser] = {
  //        cache.synchronized({
  //            cache.remove(id)
  //        })
  //    }

  def hashPassword(secret: String, passwordSalt: String): String = Crypto.secureHash(passwordSalt + secret)

  /** Check the password against the hashed password. */
  def validatePassword(secret: String, hashedPassword: String, passwordSalt: String): Boolean = {
    hashPassword(secret, passwordSalt).equals(hashedPassword)
  }

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

  def main(args: Array[String]): Unit = {
    val confValid = Config.validate
    DbSetup.init
    def fakeRole(request: Request, response: Response): UserRole.Value = UserRole.admin
    val av = new AuthenticationVerifier(fakeRole _)
    println("check " + verifierResultToString(av.check("jim", "foo")))
    println("check " + verifierResultToString(av.check("irrer@med.umich.edu", "aaaaa")))
  }

}
