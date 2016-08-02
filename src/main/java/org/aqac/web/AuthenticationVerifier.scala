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

class AuthenticationVerifier extends Verifier {

    private class Credentials(val secret: String) {
        val timeout = System.currentTimeMillis + Config.AuthenticationTimeoutInMs
    }

    private val cache = scala.collection.mutable.HashMap[String, Credentials]()

    private def put(id: String, secret: String): Unit = {
        cache.synchronized({
            cache.put(id, new Credentials(secret))
        })
    }

    private def clean: Unit = {
        cache.synchronized({
            val now = System.currentTimeMillis
            val expired = cache.filter(c => c._2.timeout < now).map(c1 => c1._1)
            cache --= expired
        })
    }

    private def checkCached(id: String, secret: String): Int = {
        clean
        cache.synchronized({
            val cred = cache.get(id)
            0 match {
                case _ if (!cred.isDefined) => Verifier.RESULT_UNKNOWN
                case _ if cred.get.secret.equals(secret) => Verifier.RESULT_VALID
                case _ => Verifier.RESULT_INVALID
            }
        })
    }

    private def checkDatabase(id: String, secret: String): Int = {
        val userList = User.list.filter(u => u.email.equalsIgnoreCase(id))
        if (userList.isEmpty) Verifier.RESULT_UNKNOWN
        else {
            val user = userList.head
            if (AuthenticationVerifier.validatePassword(secret, user.hashedPassword, user.passwordSalt)) {
                put(id, secret)
                Verifier.RESULT_VALID
            }
            else Verifier.RESULT_INVALID
        }
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
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity("try more hard", MediaType.TEXT_PLAIN) // TODO make better message
        val cr = request.getChallengeResponse
        val result = {
            if (cr != null) {
                val id = cr.getIdentifier
                val secret = new String(cr.getSecret)
                check(cr.getIdentifier, new String(cr.getSecret))
            }
            else
                Verifier.RESULT_MISSING
        }

        if (result != Verifier.RESULT_VALID) {
            val user = if (request.getChallengeResponse != null) request.getChallengeResponse.getIdentifier else "unknown"
            logWarning("user " + user + " failed to log in.  Status: " + AuthenticationVerifier.verifierResultToString(result))
        }

        //result
        Verifier.RESULT_VALID  // TODO remove! when user editing and password setting is working
    }
}

object AuthenticationVerifier {

    def hashPassword(secret: String, passwordSalt: String): String = Util.secureHash(passwordSalt + secret)

    /** Check the password against the hashed password. */
    def validatePassword(secret: String, hashedPassword: String, passwordSalt: String): Boolean = {
        hashPassword(secret, passwordSalt).equals(hashedPassword)
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
        val av = new AuthenticationVerifier
        println("check " + verifierResultToString(av.check("jim", "foo")))
        println("check " + verifierResultToString(av.check("irrer@med.umich.edu", "aaaaa")))
    }

}