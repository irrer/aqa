package org.aqac.web

import org.restlet.security.Verifier
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import org.restlet.data.MediaType
import org.aqac.Config
import org.aqac.db.User
import org.aqac.Util
import org.aqac.db.DbSetup
import org.aqac.Logging._
import org.restlet.engine.header.ChallengeWriter
import org.aqac.db.UserRole

class AuthenticationVerifier(getRequestedRole: (Request, Response) => UserRole.Value) extends Verifier {

    private def checkCached(id: String, secret: String): Int = {
        AuthenticationVerifier.clean
        val cred = AuthenticationVerifier.getFromCache(id)
        0 match {
            case _ if (!cred.isDefined) => Verifier.RESULT_UNKNOWN
            case _ if cred.get.secret.equals(secret) => Verifier.RESULT_VALID
            case _ => Verifier.RESULT_INVALID
        }
    }

    private def checkDatabase(id: String, secret: String): Int = {
        val userOpt = User.getUserById(id)
        if (userOpt.isDefined) {
            val user = userOpt.get
            if (AuthenticationVerifier.validatePassword(secret, user.hashedPassword, user.passwordSalt)) {
                AuthenticationVerifier.put(id, secret, UserRole.stringToUserRole(user.role))
                Verifier.RESULT_VALID
            }
            else Verifier.RESULT_INVALID
        }
        else Verifier.RESULT_UNKNOWN
    }

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
        if (checkCached(id, secret) == Verifier.RESULT_VALID) Verifier.RESULT_VALID
        else checkDatabase(id, secret)
    }

    override def verify(request: Request, response: Response): Int = {
        val requestedRole = getRequestedRole(request, response)
        //response.setStatus(Status.SUCCESS_OK)
        //response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
        //  response.redirectSeeOther("/")  // On failure, send user to home page TODO
        val UNAUTHORIZED = 100
        if (requestedRole == UserRole.publik) Verifier.RESULT_VALID
        else {
            val cr = request.getChallengeResponse
            val result: Int = {
                if (cr == null) Verifier.RESULT_MISSING
                else {
                    val id = cr.getIdentifier
                    val secret = new String(cr.getSecret)
                    val authentication = check(cr.getIdentifier, new String(cr.getSecret))

                    val authorized: Boolean = {
                        if (authentication == Verifier.RESULT_VALID) {
                            val userRole = AuthenticationVerifier.getFromCache(id).get.role
                            (userRole.isDefined) && (userRole.get.compare(requestedRole) >= 0)
                        }
                        else false
                    }

                    if (false) { // TODO this was supposed to give the user a nice 'Not Authenticated' page, but it does not work.
                        if (authentication != Verifier.RESULT_VALID) {
                            val content = {
                                <div>Your user id or password are incorrect.</div>
                            }
                            response.setStatus(Status.CLIENT_ERROR_UNAUTHORIZED)
                            val text = WebUtil.wrapBody(content, "Not Authenticated")
                            response.setEntity(text, MediaType.TEXT_HTML)
                        }
                    }

                    if (authorized) Verifier.RESULT_VALID else UNAUTHORIZED
                }
            }

            if (result == UNAUTHORIZED) {
                val user = if (request.getChallengeResponse != null) request.getChallengeResponse.getIdentifier else "unknown"
                logWarning("user " + user + " failed to log in.  Status: " + AuthenticationVerifier.verifierResultToString(result))
                response.setStatus(Status.SUCCESS_OK) // TODO rm
                response.redirectSeeOther("/NotAuthorized")
                Verifier.RESULT_INVALID
            }
            else result
        }

        //Verifier.RESULT_VALID  // TODO remove! when user editing and password setting is working
    }
}

object AuthenticationVerifier {

    class Credentials(val secret: String, val role: Option[UserRole.Value]) {
        val timeout = System.currentTimeMillis + Config.AuthenticationTimeoutInMs
    }

    private val cache = scala.collection.mutable.HashMap[String, Credentials]()

    def put(id: String, secret: String, role: Option[UserRole.Value]): Unit = {
        cache.synchronized({
            cache.put(id, new Credentials(secret, role))
        })
    }

    def getFromCache(id: String): Option[Credentials] = {
        cache.synchronized({
            cache.get(id)
        })
    }

    def clean: Unit = {
        cache.synchronized({
            val now = System.currentTimeMillis
            val expired = cache.filter(c => c._2.timeout < now).map(c1 => c1._1)
            cache --= expired
        })
    }

    def remove(id: String): Option[Credentials] = {
        cache.synchronized({
            cache.remove(id)
        })
    }

    def hashPassword(secret: String, passwordSalt: String): String = Util.secureHash(passwordSalt + secret)

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