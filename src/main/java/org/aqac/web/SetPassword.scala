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

object SetPassword {
    val path = "/SetPassword"
    val messageTag = "Message"
}

class SetPassword extends Restlet with SubUrlRoot {

    private val continueUrlTag = "continueUrl"

    private val pageTitle = "Login"

    // TODO set up with User.userPK.  Also protect against user changing other's password.
    private val id = new WebPlainText("Id", 4, 0, "User Id")

    private val password = new WebInputPassword("Password", 4, 0, "")

    private val verifyPassword = new WebInputPassword("Verify Password", 4, 0, "")

    private def getMessage(any: Any): Elem = {
        <div>{
            {
                val optMsg = any.asInstanceOf[ValueMapT].get(message.label)
                if (optMsg.isDefined) URLDecoder.decode(optMsg.get, "UTF-8") else ""
            }
        }</div>
    }

    private val message = new WebPlainText(Login.messageTag, false, 6, 0, getMessage _)

    val continueUrl = new WebInputHidden(continueUrlTag)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val saveButton = makeButton("Login", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(pathOf, List(List(id), List(password), List(verifyPassword), List(message), List(saveButton, cancelButton)))

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
        val onlyMessage = valueMap.filter(v => v._1.equals(message.label))
        form.setFormResponse(onlyMessage, styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def save(valueMap: ValueMapT, request: Request, response: Response) = {
        val styleMap = {
            val vu = judgePassword(valueMap)
            if (vu.isEmpty) comparePasswords(valueMap) else vu
        }
        if (styleMap.isEmpty) {
            val oldUser = User.getUserById(id.getValOrEmpty(valueMap))
            if (oldUser.isDefined) {
                val newSalt = Util.randomSecureHash
                val newHashedPW = AuthenticationVerifier.hashPassword(password.getValOrEmpty(valueMap), newSalt)

                val ou = oldUser.get
                val newUser = new User(ou.userPK, ou.id, ou.fullName, ou.email, ou.institutionPK, newHashedPW, newSalt, ou.role)
                newUser.insertOrUpdate
            response.redirectSeeOther("/")
            }
        }
    }

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    override def handle(request: Request, response: Response): Unit = {
        val valueMap = getValueMap(request)
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