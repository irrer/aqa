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

package org.aqa.webrun.gapSkew

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

/**
  * Support for generating JS scripts on request.
  */
object GapSkewHistoryRestlet {
  private val path = new String((new GapSkewHistoryRestlet).pathOf)

  private val outputPKTag = "outputPK"
  private val beamNameTag = "beamName"
  private val machinePKTag = "machinePK"

  def scriptReference(beamName: String, outputPK: Long): String = {
    s"""<script src='$path?$outputPKTag=$outputPK&$beamNameTag=$beamName'></script>"""
  }

  def combinedScriptReference(outputPK: Long, machinePK: Long): String = {
    s"""<script src='$path?$outputPKTag=$outputPK&$machinePKTag=$machinePK'></script>"""
  }
}

class GapSkewHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)

      if (valueMap.contains(GapSkewHistoryRestlet.beamNameTag)) {
        val outputPK = valueMap(GapSkewHistoryRestlet.outputPKTag).toInt
        val beamName = valueMap(GapSkewHistoryRestlet.beamNameTag).replaceAll("%20", " ")
        val js = new GapSkewHistoryChart(outputPK, beamName).javascript
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
      }

      if (valueMap.contains(GapSkewHistoryRestlet.machinePKTag)) {
        val machinePK = valueMap(GapSkewHistoryRestlet.machinePKTag).toInt
        val outputPK = valueMap(GapSkewHistoryRestlet.outputPKTag).toInt
        val machine = Machine.get(machinePK).get
        val combinedChart = new GapSkewHistoryCombinedChart(outputPK, machine)
        response.setStatus(Status.SUCCESS_OK)
        response.setEntity(combinedChart.javascript, MediaType.APPLICATION_JAVASCRIPT)
      }

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
