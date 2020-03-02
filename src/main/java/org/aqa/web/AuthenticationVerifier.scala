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

  private def createUserWithLdap(id: String, secret: String): Option[User] = {
    try {
      val institutionPK = Institution.getInstitutionByRealName(Config.LdapInstitutionName).get.institutionPK.get
      val roleText = Config.LdapRole
      val userInfo = Level2Ldap.getUserInfo(id, secret).right.get
      val user = User.insertNewUser(institutionPK, id, userInfo.firstName + " " + userInfo.lastName, userInfo.email, secret, roleText)
      Some(user)
    } catch {
      case t: Throwable => {
        logger.error("Unexpected exception while automatically creating user for LDAP: " + fmtEx(t))
        None
      }
    }
  }

  //  /**
  //   * Determine if the user is valid via LDAP.  If so, then add them to the database.
  //   */
  //  private def attemptVerifyViaLdap(id: String, secret: String): Int = {
  //    Trace.trace
  //    Level2Ldap.getGroupListOfUser(id, secret) match {
  //      case Right(groupSet) => {
  //        if (groupSet.toSeq.intersect(Config.LdapGroupList).isEmpty) {
  //          logger.warn("Unable to authenticate user via LDAP.  User is not a member of one of the configured LdapGroupList.\n" +
  //            "    User's group list: " + groupSet.mkString("    ") +
  //            "    Configured group list: " + Config.LdapGroupList.mkString("    "))
  //          Verifier.RESULT_UNKNOWN
  //        } else {
  //          // User is authenticated and authorized.  Automatically create a user using LDAP information.
  //          Trace.trace
  //          createUserWithLdap(id, secret)
  //          Trace.trace
  //          Verifier.RESULT_VALID
  //        }
  //      }
  //      case Left(errorMessage) => {
  //        logger.warn("Unable to authenticate user via LDAP")
  //        Verifier.RESULT_UNKNOWN
  //      }
  //    }
  //  }

  override def verify(request: Request, response: Response): Int = {
    val requestedRole = getRequestedRole(request, response)
    if (requestedRole.id == UserRole.publik.id) Verifier.RESULT_VALID // let anyone into public areas
    else {
      request.getChallengeResponse match {
        case null => Verifier.RESULT_MISSING
        case challResp => {
          val id = challResp.getIdentifier
          val secret = new String(challResp.getSecret)

          def isValidLdapUser = {
            Config.LdapUrl.isDefined && Level2Ldap.getUserInfo(id, secret).isRight
          }

          def addLdapUserToCache(user: User) = {
            val encyrptedPassword = AnonymizeUtil.encryptWithNonce(user.institutionPK, secret)
            val hashedPassword2 = AuthenticationVerifier.hashPassword(secret, user.passwordSalt)
            val user2 = user.copy(hashedPassword = hashedPassword2)
            CachedUser.put(id, user2)
            Verifier.RESULT_VALID
          }

          val cachedUser = CachedUser.get(id)

          val status: Int = {
            if (cachedUser.isDefined) {
              val user = cachedUser.get
              if (AuthenticationVerifier.validatePassword(secret, user.hashedPassword, user.passwordSalt))
                Verifier.RESULT_VALID
              else {
                if (isValidLdapUser) addLdapUserToCache(user)
                else Verifier.RESULT_UNKNOWN
              }
            } else {
              if (isValidLdapUser) {
                // This user is not in the database but is valid according to LDAP, so insert them into the database
                createUserWithLdap(id, secret) match {
                  case Some(user) => {
                    CachedUser.put(id, user) // add them to the cache for next time
                    Verifier.RESULT_VALID
                  }
                  case _ => Verifier.RESULT_UNKNOWN
                }
              } else
                Verifier.RESULT_UNKNOWN
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
}
