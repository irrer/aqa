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

package org.aqa.webrun.phase2.wedge

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.WedgePoint
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRun
import org.aqa.web.WebUtil._
import org.aqa.webrun.phase2.wedge.WedgeUseAsBaseline._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.PrettyPrinter

object WedgeUseAsBaseline {
  val path = new String((new WedgeUseAsBaseline).pathOf)
  val wedgePointPKTag = "wedgePointPK"
  val loadCheckboxTag = "loadCheckbox"
  val confirmTag = "confirm"
}

class WedgeUseAsBaseline extends Restlet with SubUrlRun with Logging {

  /**
    * Toggle the baseline status of the given wedge point in the database.
    * @param wedgePointPK Wedge point to flip.
    * @return
    */
  private def setBaseline(wedgePointPK: Long): Unit = {

    val wedgePoint = WedgePoint.get(wedgePointPK).get

    val newWedgePoint = wedgePoint.copy(isBaseline_text = (!wedgePoint.isBaseline).toString, isBaseline = (!wedgePoint.isBaseline))
    newWedgePoint.insertOrUpdate()
  }

  /**
    * Tell the user they did something wrong.  This should only happen if the URL was messed up or
    * there was an attacker trying strange things.
    * @param response Put message here.
    */
  private def noOutput(response: Response): Unit = {
    badRequest(response, "The wedge point designation was missing.", Status.CLIENT_ERROR_BAD_REQUEST)
  }

  /**
    * Tell the user they are not authorized.
    * @param response Put message here.
    */
  private def authorizationFailed(response: Response): Unit = {
    badRequest(response, "You are not authorized to change a baseline for a different institution.", Status.CLIENT_ERROR_UNAUTHORIZED)
  }

  /**
    * Determine if the user is authorized to change a baseline.  The user must either belong to the same
    * institution or be whitelisted.
    *
    * @param request User request, used to identify user.
    * @param wedgePointPK Wedge point to use/not use as baseline.
    * @return True if user is authorized.
    */
  private def authorized(request: Request, wedgePointPK: Long): Boolean = {
    try {
      val user = getUser(request).get

      def userIsFromInstitution(): Boolean = {
        val wedgePoint = WedgePoint.get(wedgePointPK).get
        val output = Output.get(wedgePoint.outputPK).get
        val machine = Machine.get(output.machinePK.get).get
        user.institutionPK == machine.institutionPK
      }

      userIsFromInstitution() || userIsWhitelisted(user.id)
    } catch {
      case t: Throwable =>
        logger.warn("Invalid user or other data: " + fmtEx(t))
        false
    }
  }

  def showCheckbox(response: Response, wedgePointPK: Long): Unit = {
    val wedgePoint = WedgePoint.get(wedgePointPK).get

    val input = {
      val onClick = "setBaselineState(this, " + wedgePointPK + ")"
      if (wedgePoint.isBaselineX) {
        <input class="form-control" type="checkbox" name="Baseline" onclick={onClick} value="true" checked="true"/>
      } else {
        <input class="form-control" type="checkbox" name="Baseline" onclick={onClick} value="false"/>
      }
    }

    val elem = {
      <div>
        <label title="Check to use this beam as a baseline.">{input}Baseline</label>
      </div>
    }

    val content = new PrettyPrinter(1024, 2).format(elem)

    setResponse(content, response, Status.SUCCESS_OK)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap = getValueMap(request)

    try {
      0 match {
        case _ if valueMap.contains(wedgePointPKTag) =>
          val wedgePointPK = valueMap(wedgePointPKTag).toLong
          if (authorized(request, wedgePointPK)) {
            setBaseline(wedgePointPK)
            val html = WebUtil.wrapBody(<div>baseline toggled</div>, "Set Baseline for Symmetry and Flatness")
            WebUtil.setResponse(html, response, Status.SUCCESS_OK)
          } else
            authorizationFailed(response)

        case _ if valueMap.contains(loadCheckboxTag) =>
          val wedgePointPK = valueMap(loadCheckboxTag).toLong
          showCheckbox(response, wedgePointPK)

        case _ => noOutput(response)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }

}
