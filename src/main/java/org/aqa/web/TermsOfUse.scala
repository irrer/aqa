package org.aqa.web

import org.restlet.security.ChallengeAuthenticator
import org.restlet.data.ChallengeScheme
import org.restlet.data.ChallengeRequest
import org.restlet.data.ChallengeResponse
import org.restlet.Context
import org.restlet.Request
import org.restlet.Response
import org.restlet.security.Verifier
import org.restlet.Restlet
import org.aqa.web.WebUtil._
import org.aqa.db.User
import org.restlet.data.Status
import scala.xml.Elem
import java.net.URLDecoder
import org.restlet.engine.security.AuthenticatorHelper
import org.restlet.engine.security.HttpBasicHelper
import org.restlet.engine.header.ChallengeWriter
import org.restlet.data.Header
import org.restlet.util.Series
import org.restlet.security.SecretVerifier
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.db.CachedUser
import java.sql.Timestamp

object TermsOfUse {
    val path = "/TermsOfUse"
}

class TermsOfUse extends Restlet with SubUrlRoot {

    private val continueUrlTag = "continueUrl"

    private val pageTitle = "Term Of Use"

    private val id = new WebInputText("Id", 4, 0, "User Id")

    private val password = new WebInputPassword("Password", 4, 0, "")

    private def getTermsOfUse(valueMap: ValueMapT): Elem = {
        <div>{
            Config.TermsOfUse
        }</div>
    }

    private val message = new WebPlainText(Login.messageTag, false, 6, 0, getTermsOfUse _)

    val continueUrl = new WebInputHidden(continueUrlTag)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val agreeButton = makeButton("Agree", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(pathOf, List(List(id), List(password), List(message), List(continueUrl), List(agreeButton, cancelButton)))

    private def emptyForm(response: Response) = {
        form.setFormResponse(emptyValueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def agree(response: Response) = {
        CachedUser.get(response.getRequest) match {
            case Some(user) => {
                val timestamp = new Timestamp(System.currentTimeMillis)
                user.updateTermsOfUseAcknowledgment(Some(timestamp))
                response.redirectSeeOther("/Agreed") // TODO should show an acknowledgement, and send them to where they were originally going
            }
            case _ =>
        }
    }

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, agreeButton) => agree(response)
                case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, "Unexpected failure: " + fmtEx(t))
            }
        }
    }

}
