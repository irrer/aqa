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
import org.aqa.db.OutputApproval
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.getValueMap
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.User
import org.aqa.web.WebUtil.getUser
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import java.sql.Timestamp

class ApprovalChangeRestlet extends Restlet with SubUrlRoot with Logging {

  private def makeResponse(approval: Option[OutputApproval]): String = {
    val responseText: String =
      if (approval.isEmpty) {
        s"${OutputApproval.UNAPPROVED.name}\n\n"
      } else {
        val status = approval.get.status
        val dateText = Util.formatDate(ApprovalHtml.dateFormat, approval.get.creationDateTime)
        val userId = User.get(approval.get.userPK).get.id

        s"$status\n$userId\n$dateText"
      }

    responseText
  }

  private def isApprover(request: Request): Boolean = {
    val user = WebUtil.getUser(request)
    user.isDefined && user.get.isApprover
  }

  private def changeApproval(outputPK: Long, request: Request, status: String): OutputApproval = {

    val now = new Timestamp(System.currentTimeMillis())

    val userPK = getUser(request).get.userPK.get

    val approval = new OutputApproval( //
      outputApprovalPK = None,
      outputPK = outputPK,
      creationDateTime = now,
      userPK = userPK,
      status = status
    )

    val newApproval = approval.insert

    logger.info(s"New approval inserted: $newApproval")

    newApproval
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(ApprovalChangeRestlet.outputPKTag).toLong
      val status = valueMap(ApprovalChangeRestlet.statusTag)
      val output = Output.get(outputPK)

      val oldApproval = OutputApproval.getByOutput(outputPK).lastOption

      val responseText: String = 0 match {

        // invalid output - do nothing
        case _ if output.isEmpty =>
          makeResponse(oldApproval)

        // invalid status - do nothing
        case _ if OutputApproval.stringToStatus(status).isEmpty =>
          makeResponse(oldApproval)

        // same status - do nothing
        case _ if oldApproval.isDefined && oldApproval.get.status.equalsIgnoreCase(status) =>
          makeResponse(oldApproval)

        // currently UNAPPROVED by default, do not allow changing to UNAPPROVED.
        case _ if (oldApproval.isEmpty || oldApproval.get.status.equalsIgnoreCase(OutputApproval.UNAPPROVED.name)) && status.equals(OutputApproval.UNAPPROVED.name) =>
          makeResponse(oldApproval)

        // user is not authorized - do nothing
        case _ if !isApprover(request) =>
          makeResponse(oldApproval)

        case _ =>
          val newApproval = changeApproval(outputPK, request, status)
          makeResponse(Some(newApproval))
      }

      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(responseText, MediaType.TEXT_PLAIN)

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}

object ApprovalChangeRestlet extends Logging {

  val outputPKTag: String = "outputPK"
  val statusTag: String = "status"

  private val path = new String((new ApprovalChangeRestlet).pathOf)
  def makeReference(outputPK: Long): String = {
    "<script src='" + path + "?outputPK=" + outputPK + "'></script>"
  }
}
