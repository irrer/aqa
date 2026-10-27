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

package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.web.WebUtil._
import org.aqa.Config
import org.aqa.Logging
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

/**
  * Support for generating JS scripts on request.
  */
object SymmetryAndFlatnessHistoryRestlet {
  private val path = new String((new SymmetryAndFlatnessHistoryRestlet).pathOf)

  def makeReference(beamName: String, outputPK: Long, span_mm: Double, withPsm: Boolean): String = {
    s"<script src='$path?" +
      s"${SymmetryAndFlatnessSubHTML.outputPKTag}=$outputPK&amp;" +
      s"${SymmetryAndFlatnessSubHTML.beamNameTag}=$beamName&amp;" +
      s"${SymmetryAndFlatnessSubHTML.withPsmTag}=${withPsm.toString}&amp;" +
      s"${SymmetryAndFlatnessSubHTML.spanTag}=${span_mm.round}" +
      s"'></script>"
  }
}

class SymmetryAndFlatnessHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(SymmetryAndFlatnessSubHTML.outputPKTag).toInt
      val beamName = valueMap(SymmetryAndFlatnessSubHTML.beamNameTag).replaceAll("%20", " ")
      val withPsm = {
        valueMap.contains(SymmetryAndFlatnessSubHTML.withPsmTag) &&
        valueMap(SymmetryAndFlatnessSubHTML.withPsmTag).toBoolean
      }

      val span_mm: Long = {
        if (valueMap.contains(SymmetryAndFlatnessSubHTML.spanTag))
          valueMap(SymmetryAndFlatnessSubHTML.spanTag).toLong
        else
          Config.SymmetrySpan_mm.round

      }

      val js = new SymmetryAndFlatnessBeamHistoryHTML(beamName, outputPK, withPsm, span_mm).javascript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
