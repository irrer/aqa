/*
 * Copyright 2025 Regents of the University of Michigan
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

package org.aqa.approval

import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.internalFailure
import org.aqa.Logging
import org.aqa.db.User
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.getValueMap
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

class SetApproverStateRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val invokingUser = WebUtil.getUser(request)

      val userToChangePK = valueMap(SetApproverStateRestlet.userPKTag).toLong

      val status = valueMap.get(SetApproverStateRestlet.stateTag)
      val userToChange = User.get(userToChangePK)

      def goodResponse(state: Boolean): Unit = {
        val text = if (state) User.approverTag else ""
        val updatedUser = userToChange.get.copy(authorizations = Some(text))
        updatedUser.insertOrUpdate()
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity(state.toString, MediaType.TEXT_PLAIN)
      }

      def badResponse(): Unit = {
        response.setStatus(Status.CLIENT_ERROR_BAD_REQUEST)
        response.setEntity("", MediaType.TEXT_PLAIN)

      }

      0 match {

        // User not logged in
        case _ if invokingUser.isEmpty =>
          badResponse()

        // Invalid user - do nothing
        case _ if userToChange.isEmpty =>
          badResponse()

        // User is not authorized to change approvals
        case _ if !invokingUser.get.isApprover =>
          badResponse()

        // User can not change their own state.  This is for their protection.
        case _ if invokingUser.get.userPK.get == userToChange.get.userPK.get =>
          badResponse()

        // User is not authorized to change approvals
        case _ if status.isEmpty =>
          badResponse()

        case _ =>
          goodResponse(status.isDefined && status.get.equals("true"))
      }

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}

object SetApproverStateRestlet extends Logging {

  val userPKTag: String = "userPK"
  private val stateTag: String = "state"

  private val path = new String((new SetApproverStateRestlet).pathOf)
  def makeReference(outputPK: Long): String = {
    "<script src='" + path + "?outputPK=" + outputPK + "'></script>"
  }
}
