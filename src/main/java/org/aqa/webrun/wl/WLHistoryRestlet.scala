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

package org.aqa.webrun.wl

import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

/**
  * Support for generating JS scripts on request.
  */
object WLHistoryRestlet {
  private val path = new String((new WLHistoryRestlet).pathOf)

  private val outputPKTag = "outputPK"
  private val beamNameTag = "beamName"

  def makeReference(beamName: String, outputPK: Long): String = {
    "<script src='" + path + "?" + outputPKTag + "=" + outputPK + "&amp;" + beamNameTag + "=" + beamName + "'></script>"
  }
}

class WLHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(WLHistoryRestlet.outputPKTag).toInt
      val chartList = new WLChart(outputPK).chartList
      val js = chartList.map(_.javascript).mkString("\n", "\n// ------------------------------------------------------------------------------------------\n", "\n")
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
