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

import org.aqa.db.CachedUser
import org.aqa.db.User
import org.aqa.db.UserRole
import org.aqa.web.WebUtil._
import org.aqa.Crypto
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.Elem

object SetPassword {
  val path = "/SetPassword"
  val messageTag = "Message"
}

class SetPassword extends Restlet with SubUrlRoot {

  private val pageTitle = "Set Password"

  private def getTargetUserId(valueMap: ValueMapT): Elem = {
    val pk = valueMap.get(userPK.label)
    val usrId = if (pk.isDefined) User.get(pk.get.toLong).get.id else "unknown"
    WebUtil.wrapAlias(usrId)
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

  private val forbiddenMessage = new WebPlainText("Password reset forbidden", false, 3, 0, _ => { <div>You may not reset passwords from other institutions.</div> })
  private val formChangeForbidden = new WebForm(pathOf, List(List(forbiddenMessage), List(cancelButton)))

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
      case _ if (!targetUser.isDefined)                                    => Error.make(id, "Target user has not been defined")
      case _ if (!authnUser.isDefined)                                     => Error.make(id, "User has not been authenticated")
      case _ if isAdmin                                                    => styleNone // user is admin so they can change anyone's passord
      case _ if (authnUser.get.userPK.get.toString.equals(targetUser.get)) => styleNone // user is changing their own password
      case _                                                               => Error.make(id, "You can only change your own password unless you are an administrator")
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
        val newSalt = Crypto.randomSecureHash
        val newHashedPW = CachedUser.hashPassword(password.getValOrEmpty(valueMap), newSalt)

        val usr = User.get(pkOpt.get.toLong)
        if (usr.isDefined) {
          val origUser = usr.get
          val newUser = origUser.copy(hashedPassword = newHashedPW, passwordSalt = newSalt)
          newUser.insertOrUpdate
          // remove old credentials so that the old password will not work
          CachedUser.clear
          // replace the users' credentials in the cache
          val userId = request.getChallengeResponse.getIdentifier.toLowerCase.trim
          //CachedUser.put(userId, newUser)
          val content = {
            <div>
              The password for{origUser.id}
              has been changed.
              <p></p>
              You will need to login again with the new password.
              <p></p>
              <a href="/">Home</a>
            </div>
          }
          simpleWebPage(content, Status.SUCCESS_OK, "Password Changed", response)
          //response.redirectSeeOther("/")
        }
      }
    } else {
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
    } else
      WebUtil.internalFailure(response, "SetPassword Expected user to be authenicated (logged in) but they were not.")
  }

  private def processRequest(valueMap: ValueMapT, request: Request, response: Response): Unit = {
    val clientUser = getUser(response.getRequest)
    val user = {
      val pk = valueMap.get(userPK.label)
      if (pk.isDefined) User.get(pk.get.toLong) else None
    }

    val authOk = userIsWhitelisted(response) || (clientUser.isDefined && user.isDefined && (clientUser.get.institutionPK == user.get.institutionPK))
    0 match {
      case _ if buttonIs(valueMap, saveButton) && authOk => save(valueMap, request, response)
      case _ if buttonIs(valueMap, saveButton)           => formChangeForbidden.setFormResponse(valueMap, styleNone, pageTitle, response, Status.CLIENT_ERROR_UNAUTHORIZED)
      case _ if buttonIs(valueMap, cancelButton)         => response.redirectSeeOther("/")
      case _                                             => emptyForm(valueMap, response)
    }

  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    try {
      val valueMap = getValueMap(request)

      val userPKText = valueMap.get(userPK.label)
      if (userPKText.isDefined) {
        processRequest(valueMap, request, response)
      } else {
        getAuthenticatedUserAndRetry(valueMap, request, response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
