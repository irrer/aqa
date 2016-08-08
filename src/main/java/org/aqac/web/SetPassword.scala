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

    private val pageTitle = "Set Password"

    private def getTargetUserId(vma: Any): Elem = {
        val pk = (vma.asInstanceOf[ValueMapT]).get(userPK.label)
        val usrId = if (pk.isDefined) User.get(pk.get.toLong).get.id else "unknown"
        <div>{ usrId }</div>
    }

    private val id = new WebPlainText("Id", true, 4, 0, getTargetUserId _)

    private val password = new WebInputPassword("Password", 4, 0, "")

    private val verifyPassword = new WebInputPassword("Verify Password", 4, 0, "")

    val userPK = new WebInputHidden(UserUpdate.userPKTag)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(pathOf, List(List(id), List(password), List(verifyPassword), List(saveButton, cancelButton, userPK)))

    /**
     * Only permit high quality passwords.
     */
    private def judgePassword(valueMap: ValueMapT): StyleMapT = {
        val err = AuthenticationVerifier.judgePassword(valueMap.get(password.label).get)
        if (err.isDefined) Error.make(password, err.get) else styleNone
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
        val targetUser = valueMap.get(userPK.label)
        val authnUser = getUser(request)
        val role = if (authnUser.isDefined) UserRole.stringToUserRole(authnUser.get.role) else None
        val isAdmin = role.isDefined && (role.get == UserRole.admin)

        0 match {
            case _ if (!targetUser.isDefined) => Error.make(id, "Target user has not been defined")
            case _ if (!authnUser.isDefined) => Error.make(id, "User has not been authenticated")
            case _ if isAdmin => styleNone // user is admin so they can change anyone's passord
            case _ if (authnUser.get.userPK.get.toString.equals(targetUser.get)) => styleNone // user is changing their own password
            case _ => Error.make(id, "You can only change your own password unless you are an administrator")
        }
    }

    private def validate(valueMap: ValueMapT, request: Request): StyleMapT = {
        val authz = authorized(valueMap, request)
        if (!authz.isEmpty) authz
        else {
            val vu = judgePassword(valueMap)
            if (!vu.isEmpty) vu
            else comparePasswords(valueMap)
        }
    }

    private def save(valueMap: ValueMapT, request: Request, response: Response) = {
        val styleMap = validate(valueMap, request)
        if (styleMap.isEmpty) {
            val pkOpt = valueMap.get(userPK.label)
            if (pkOpt.isDefined) {
                val newSalt = Util.randomSecureHash
                val newHashedPW = AuthenticationVerifier.hashPassword(password.getValOrEmpty(valueMap), newSalt)

                val usr = User.get(pkOpt.get.toLong)
                if (usr.isDefined) {
                    val ou = usr.get
                    val newUser = new User(ou.userPK, ou.id, ou.fullName, ou.email, ou.institutionPK, newHashedPW, newSalt, ou.role)
                    newUser.insertOrUpdate
                    // remove old credentials so that the old password will not work
                    AuthenticationVerifier.remove(ou.id)
                    // replace the users' credentials in the cache
                    AuthenticationVerifier.put(ou.id, valueMap.get(password.label).get, UserRole.stringToUserRole(ou.role))
                    val content = {
                        <div>
                            The password for { ou.id } has been changed.
                            <p></p>
                            You will need to re-login.
                            <p></p>
                            <a href="/">Home</a>
                        </div>
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
     * Use the current (authenticated) user.
     */
    private def getAuthenticatedUserAndRetry(valueMap: ValueMapT, request: Request, response: Response): Unit = {
        val authUser = getUser(request)
        if (authUser.isDefined) {
            val path = SetPassword.path + "?" + userPK.label + "=" + authUser.get.userPK.get
            response.redirectSeeOther(path)
        }
        else
            WebUtil.internalFailure(response, "SetPassword Expected user to be authenicated (logged in) but they were not.")
    }

    private def processRequest(valueMap: ValueMapT, request: Request, response: Response): Unit = {
        0 match {
            case _ if buttonIs(valueMap, saveButton) => save(valueMap, request, response)
            case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
            case _ => emptyForm(valueMap, response)
        }

    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        try {
            val valueMap = getValueMap(request)

            val userPKText = valueMap.get(userPK.label)
            if (userPKText.isDefined) {
                processRequest(valueMap, request, response)
            }
            else {
                getAuthenticatedUserAndRetry(valueMap, request, response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }

}
