package org.aqac.web

import org.restlet.security.ChallengeAuthenticator
import org.restlet.data.ChallengeScheme
import org.restlet.data.ChallengeRequest
import org.restlet.data.ChallengeResponse
import org.restlet.Context
import org.restlet.Request
import org.restlet.Response
import org.restlet.security.Verifier
import org.restlet.Restlet
import org.aqac.web.WebUtil._
import org.aqac.db.User
import org.restlet.data.Status
import scala.xml.Elem
import java.net.URLDecoder
import org.restlet.engine.security.AuthenticatorHelper
import org.restlet.engine.security.HttpBasicHelper
import org.restlet.engine.header.ChallengeWriter
import org.restlet.data.Header
import org.restlet.util.Series
import org.aqac.Util
import org.aqac.db.UserRole

object SetPassword {
    val path = "/SetPassword"
    val messageTag = "Message"
}

class SetPassword extends Restlet with SubUrlRoot {

    private val continueUrlTag = "continueUrl"

    private val pageTitle = "Set Password"

    private def getTargetUserId(vma: Any): Elem = {
        val userId = (vma.asInstanceOf[ValueMapT]).get(id.label)
        <div>{ if (userId.isDefined) userId.get else "unknown" }</div>
    }

    private val id = new WebPlainText("Id", true, 4, 0, getTargetUserId _)

    private val password = new WebInputPassword("Password", 4, 0, "")

    private val verifyPassword = new WebInputPassword("Verify Password", 4, 0, "")

    val continueUrl = new WebInputHidden(continueUrlTag)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(pathOf, List(List(id), List(password), List(verifyPassword), List(saveButton, cancelButton)))

    val minPasswordSize = 8

    private def containsNonAlpha(passwordText: String): Boolean = passwordText.toLowerCase.replaceAll("[a-z]", "").size > 0

    /**
     * Only permit high quality passwords.
     */
    private def judgePassword(valueMap: ValueMapT): StyleMapT = {
        val text = valueMap.get(password.label).get
        0 match {
            case _ if (text.size < minPasswordSize) => Error.make(password, "Password must be at least " + minPasswordSize + " characters long.")
            case _ if (!containsNonAlpha(text)) => Error.make(password, "Password must contain at least one non-alpha character.")
            case _ => styleNone // good enough
        }
    }

    /**
     * Compare passwords to make sure they are the same.
     */
    private def comparePasswords(valueMap: ValueMapT): StyleMapT = {
        val text1 = valueMap.get(password.label).get
        val text2 = valueMap.get(verifyPassword.label).get

        if (text1.equals(text2)) styleNone
        else Error.make(verifyPassword, "Passwords do not match")
    }

    /**
     * Show this when asks to change password.
     */
    private def emptyForm(valueMap: ValueMapT, response: Response) = {
        form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def authorized(valueMap: ValueMapT, request: Request): StyleMapT = {
        val targetUser = valueMap.get(id.label) // change password of this user
        val authnUser = getUser(request)
        val role = if (authnUser.isDefined) UserRole.stringToUserRole(authnUser.get.role) else None
        val isAdmin = role.isDefined && (role.get == UserRole.admin)

        0 match {
            case _ if (!targetUser.isDefined) => Error.make(id, "Target user has not been defined")
            case _ if (!authnUser.isDefined) => Error.make(id, "User has not been authenticated")
            case _ if isAdmin => styleNone
            case _ if (authnUser.get.id.equals(targetUser.get)) => styleNone
            case _ => Error.make(id, "You can only change your own password unless you are an administrator")
        }
    }

    private def save(valueMap: ValueMapT, request: Request, response: Response) = {
        val styleMap = {
            val authz = authorized(valueMap, request)
            if (!authz.isEmpty) authz
            else {
                val vu = judgePassword(valueMap)
                if (!vu.isEmpty) vu
                else comparePasswords(valueMap)
            }
        }

        if (styleMap.isEmpty) {
            val oldUser = valueMap.get(id.label)
            if (oldUser.isDefined) {
                val newSalt = Util.randomSecureHash
                val newHashedPW = AuthenticationVerifier.hashPassword(password.getValOrEmpty(valueMap), newSalt)

                val odb = User.getUserById(oldUser.get)
                if (odb.isDefined) {
                    val ou = odb.get
                    val newUser = new User(ou.userPK, ou.id, ou.fullName, ou.email, ou.institutionPK, newHashedPW, newSalt, ou.role)
                    newUser.insertOrUpdate
                    AuthenticationVerifier.remove(ou.id) // remove the users' credentials from the cache
                    val content = {
                        <center>
                            You will be required to re-login with the new password.
                            <p></p>
                            <a href="/">Home</a>
                        </center>
                    }
                    simpleWebPage(content, Status.SUCCESS_OK, "Password Changed", response)
                }
            }
        }
        else {
            form.setFormResponse(valueMap, styleMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    /**
     * Try to get the target user from the valueMap, and if that fails use the current (authenticated) user.
     */
    private def getTargetUser(valueMap: ValueMapT, request: Request): Option[String] = {
        val t = valueMap.get(id.label)
        if (t.isDefined) t
        else {
            val authUser = getUser(request)
            if (authUser.isDefined) Some(authUser.get.id)
            else None
        }
    }

    private def initUser(valueMap: ValueMapT, request: Request): ValueMapT = {
        val targetUser = getTargetUser(valueMap, request)
        if (targetUser.isEmpty) valueMap
        else {
            val authnUser = getUser(request)
            if (authnUser.isDefined) valueMap + (id.label -> authnUser.get.id)
            else valueMap
        }
    }

    override def handle(request: Request, response: Response): Unit = {
        val valueMap = initUser(getValueMap(request), request)

        try {
            0 match {
                case _ if buttonIs(valueMap, saveButton) => save(valueMap, request, response)
                case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
                case _ => emptyForm(valueMap, response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }

}