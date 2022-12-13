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
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

class NotAuthorized extends Restlet with SubUrlRoot {

  private val pageTitle = "Not Authorized"

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val homeButton = makeButton("Home", true, ButtonType.BtnPrimary)

  private val content = "Your user id and password are correct, but you are not authorized to use this part of the web site."

  private val message = new WebPlainText("message", false, 6, 0, _ => <div>{content}</div>)

  private val form = new WebForm(pathOf, List(List(message), List(homeButton)))

  private def emptyForm(response: Response) = {
    form.setFormResponse(emptyValueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
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
        case _ if buttonIs(valueMap, homeButton) => response.redirectSeeOther("/")
        case _                                   => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }
}
