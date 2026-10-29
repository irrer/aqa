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
import org.aqa.Logging
import org.aqa.db.SymmetryAndFlatness
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

  def makeReference(beamName: String, outputPK: Long, span_mm: Double, withPsm: Boolean, diameter_mm: Double): String = {
    s"<script src='$path?" +
      s"${SymmetryAndFlatnessSubHTML.outputPKTag}=$outputPK&amp;" +
      s"${SymmetryAndFlatnessSubHTML.beamNameTag}=$beamName&amp;" +
      s"${SymmetryAndFlatnessSubHTML.withPsmTag}=${withPsm.toString}&amp;" +
      s"${SymmetryAndFlatnessSubHTML.spanTag}=${span_mm.round}&amp;" +
      s"${SymmetryAndFlatnessSubHTML.diameterTag}=${diameter_mm.round}" +
      s"'></script>"
  }
}

class SymmetryAndFlatnessHistoryRestlet extends Restlet with SubUrlRoot with Logging {

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val outputPK = valueMap(SymmetryAndFlatnessSubHTML.outputPKTag).toInt
      val beamName = {
        val vm = valueMap.map(kv => (kv._1.toLowerCase(), kv._2))
        vm(SymmetryAndFlatnessSubHTML.beamNameTag.toLowerCase()).replaceAll("%20", " ")
      }

      val withPsm = {
        valueMap.contains(SymmetryAndFlatnessSubHTML.withPsmTag) &&
        valueMap(SymmetryAndFlatnessSubHTML.withPsmTag).toBoolean
      }

      // Get a list of beams for this output if needed.  For legacy data there will be one, for later data there will be two.
      lazy val symFlatList = SymmetryAndFlatness.getByOutput(outputPK).filter(sf => sf.beamName.equalsIgnoreCase(beamName))

      val span_mm: Long = {
        if (valueMap.contains(SymmetryAndFlatnessSubHTML.spanTag))
          valueMap(SymmetryAndFlatnessSubHTML.spanTag).toLong
        else {
          val spanList = symFlatList.flatMap(_.span_mm).sorted.map(_.round)
          spanList.head
        }
      }

      val diameter_mm: Long = {
        if (valueMap.contains(SymmetryAndFlatnessSubHTML.diameterTag))
          valueMap(SymmetryAndFlatnessSubHTML.diameterTag).toLong
        else {
          val diameterList = symFlatList.flatMap(_.diameter_mm).sorted.map(_.round)
          diameterList.head
        }
      }

      val js = new SymmetryAndFlatnessBeamHistoryHTML(beamName, outputPK, withPsm, span_mm, diameter_mm).javascript
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(js, MediaType.APPLICATION_JAVASCRIPT)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
