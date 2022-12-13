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

import org.aqa.web.WebUtil._
import org.aqa.Config
import org.aqa.db.CachedUser
import org.restlet.data.ChallengeRequest
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.sql.Timestamp
import java.util.concurrent.CopyOnWriteArrayList
import scala.xml.Elem

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
        CachedUser.clear
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
        case _ if buttonIs(valueMap, agreeButton)  => agree(request, response)
        case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
        case _                                     => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
