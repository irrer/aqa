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

package org.aqa.webrun.phase2.collimatorCentering

import org.aqa.Logging
import org.aqa.web.WebUtil._
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringChartHistoryRestlet.gantryAngleTag
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringChartHistoryRestlet.outputPKTag
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

/**
  * Support a Restlet for getting the latest history for collimator centering as a chart.
  */
object CollimatorCenteringChartHistoryRestlet {
  private val path = new String((new CollimatorCenteringChartHistoryRestlet).pathOf)

  private val outputPKTag = "outputPK"
  private val gantryAngleTag = "GantryAngle"

  def makeReference(outputPK: Long, gantryAngle: Int): String = {
    s"<script src='$path?$outputPKTag=$outputPK&$gantryAngleTag=$gantryAngle></script>"
  }
}

class CollimatorCenteringChartHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(outputPKTag).toLong
      val gantryAngle = {
         valueMap.get(gantryAngleTag) match {
           case Some(text) => text.toInt
           case _ => 0 // for backwards compatibility.  Old web pages will not have the gantry angle
         }
      }
      val cdc = new CollimatorCenteringChart(outputPK, gantryAngle)
      val js = cdc.javascript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
