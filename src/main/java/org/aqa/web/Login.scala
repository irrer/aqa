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

object Login {
    val path = "/Login"
    val messageTag = "Message"
}

class Login extends Restlet with SubUrlRoot {

    private val continueUrlTag = "continueUrl"

    private val pageTitle = "Login"

    private val id = new WebInputText("Id", 4, 0, "User Id")

    private val password = new WebInputPassword("Password", 4, 0, "")

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

    private val loginButton = makeButton("Login", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(pathOf, List(List(id), List(password), List(message), List(continueUrl), List(loginButton, cancelButton)))

    private def validateUser(valueMap: ValueMapT): StyleMapT = {
        val idText = valueMap.get(id.label).get
        val user = User.getUserById(idText)
        if (user.isDefined) styleNone else Error.make(id, "No such user")
    }

    /**
     * Verify id and password.
     */
    private def validatePassword(valueMap: ValueMapT): StyleMapT = {
        val idText = valueMap.get(id.label).get
        val user = User.getUserById(idText).get
        val enteredPasswordText = valueMap.get(password.label).get

        val ok = AuthenticationVerifier.validatePassword(enteredPasswordText, user.hashedPassword, user.passwordSalt)

        if (ok) styleNone else Error.make(password, "Incorrect password")
    }

    /**
     * Show this when user asks to log in.
     */
    private def emptyForm(valueMap: ValueMapT, response: Response) = {
        val onlyMessage = valueMap.filter(v => v._1.equals(message.label))
        form.setFormResponse(onlyMessage, styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def redirect(valueMap: ValueMapT, response: Response) {
        val continueUrlText = valueMap.get(continueUrl.label)
        val path = if (continueUrlText.isDefined && (continueUrlText.get.size > 0)) continueUrlText.get else "/"
        response.redirectSeeOther(path)
    }

    /**
     * Get user id and password and set up credentials.
     */
    private def login(valueMap: ValueMapT, request: Request, response: Response) = {
        val styleMap = {
            val vu = validateUser(valueMap)
            if (vu.isEmpty) validatePassword(valueMap) else vu
        }

        if (true) {     // TODO rm
            val cr = request.getChallengeResponse
            if (cr != null)
                println("login ident: " + cr.getIdentifier + "    secret: " + new String(cr.getSecret))
                else println("login ident no credentials")
        }

        if (false) { // TODO rm
            val crw = new ChallengeWriter
            val bh = new HttpBasicHelper
            println("hey ho")
        }

        if (true) { //   TODO put back if (styleMap.isEmpty) {
            // TODO should send credentials to client for further use
            // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

            if (false) {
                val chalResp = new ChallengeResponse(WebServer.challengeScheme)
                chalResp.setIdentifier(id.getValOrEmpty(valueMap))
                chalResp.setSecret(password.getValOrEmpty(valueMap))
                val cw = new ChallengeWriter
                val basicHelper = new HttpBasicHelper()
                val httpHeaders: Series[Header] = null
                basicHelper.formatResponse(cw, chalResp, request, httpHeaders)
            }

            if (false) {
                val chalReq = new ChallengeRequest(WebServer.challengeScheme, "Hiya there")

                val challengeRequests = new java.util.ArrayList[ChallengeRequest]()
                challengeRequests.add(chalReq)
                response.setChallengeRequests(challengeRequests)

                val entity = response.getEntity
                val authnHelper = new org.restlet.engine.security.HttpBasicHelper()
                //val authnHelperx = new org.restlet.data.AuthenticationHelper ()

                redirect(valueMap, response)
            }

            if (false) {
                val challengeRequest = new ChallengeRequest(WebServer.challengeScheme, "Hiya there")
                val challengeResponse = new ChallengeResponse(challengeRequest, response, "irrer", "foo")
                request.setChallengeResponse(challengeResponse)
            }
        }
        else {
            form.setFormResponse(valueMap, styleMap, pageTitle, response, Status.CLIENT_ERROR_UNAUTHORIZED)
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
                case _ if buttonIs(valueMap, loginButton) => login(valueMap, request, response)
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
