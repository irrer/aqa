/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
import org.aqa.db.CachedUser

object Login {
  val path = "/Login"
  val messageTag = "Message"
}

class Login extends Restlet with SubUrlRoot {

  private val continueUrlTag = "continueUrl"

  private val pageTitle = "Login"

  private val id = new WebInputText("Id", 4, 0, "User Id")

  private val password = new WebInputPassword("Password", 4, 0, "")

  private def getMessage(valueMap: ValueMapT): Elem = {
    <div>{
      valueMap.get(message.label) match {
        case Some(optMsg) => URLDecoder.decode(optMsg, "UTF-8")
        case _ => ""
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
    val user = CachedUser.get(idText)
    if (user.isDefined) styleNone else Error.make(id, "No such user")
  }

  /**
   * Verify id and password.
   */
  private def validatePassword(valueMap: ValueMapT): StyleMapT = {
    val idText = valueMap.get(id.label).get
    val user = CachedUser.get(idText).get
    val enteredPasswordText = valueMap.get(password.label).get

    val ok = CachedUser.validatePassword(user, enteredPasswordText)

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

    if (true) { // TODO rm
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
    } else {
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
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
