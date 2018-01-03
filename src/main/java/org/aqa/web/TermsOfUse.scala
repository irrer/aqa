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
import org.aqa.Config
import org.aqa.db.CachedUser
import java.sql.Timestamp
import java.util.concurrent.CopyOnWriteArrayList

object TermsOfUse {
  val path = "/TermsOfUse"
}

class TermsOfUse extends Restlet with SubUrlRoot {

  private val agreeTag = "agree"

  private val pageTitle = "Terms Of Use"

  private def getTermsOfUse(valueMap: ValueMapT): Elem = {
    if (valueMap.contains(agreeTag)) {
      <div>
        Thank you for agreeing to the terms of use.
        <p></p>
        Click on the AQA logo to continue.
      </div>
    } else {
      <div>{
        Config.TermsOfUse
      }</div>
    }
  }

  private val message = new WebPlainText(Login.messageTag, false, 6, 0, getTermsOfUse _)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val agreeButton = makeButton("Agree", true, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form(valueMap: ValueMapT) = {
    if (valueMap.contains(agreeTag)) new WebForm(pathOf, List(List(message)))
    else new WebForm(pathOf, List(List(message), List(agreeButton, cancelButton)))
  }

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form(valueMap).setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
    val cr = new CopyOnWriteArrayList[ChallengeRequest]()
  }

  private def agree(request: Request, response: Response) = {
    CachedUser.get(response.getRequest) match {
      case Some(user) => {
        CachedUser.remove(user.id)
        val timestamp = new Timestamp(System.currentTimeMillis)
        user.updateTermsOfUseAcknowledgment(Some(timestamp))
        response.redirectSeeOther(pathOf + "?" + agreeTag + "=1")
      }
      case _ => response.redirectSeeOther("/")
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
        case _ if buttonIs(valueMap, agreeButton) => agree(request, response)
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
