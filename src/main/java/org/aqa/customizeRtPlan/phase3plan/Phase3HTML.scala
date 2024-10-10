package org.aqa.customizeRtPlan.phase3plan

/*
 * Copyright 2024 Regents of the University of Michigan
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

import edu.umro.ScalaUtil.Trace
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.customizeRtPlan.phase3plan.Phase3HtmlForm._
import org.aqa.customizeRtPlan.phase3plan.Phase3JS.createPlanTag
import org.aqa.db.Machine
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.Restlet
import org.restlet.data.Status
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method

import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML

object Phase3HTML extends Restlet with SubUrlRoot with Logging {}

class Phase3HTML extends Restlet with SubUrlRoot with Logging {

  /**
    * Make a list of all the checked beams.
    *
    * @return List of checked beams.
    */
  private def makeCheckedBeamList(doc: Elem, subProcedureList: SubProcedureList): Seq[Beam] = {

    def toBeam(node: Node): Option[Beam] = {
      if ((node \ "checked").text.toBoolean) {
        val id = (node \ "id").text
        val beam = subProcedureList.beamList.find(b => b.beamName.equals(id))
        beam
      } else
        None
    }

    val checkedBeamList = (doc \ "Checkbox").flatMap(toBeam)

    checkedBeamList
  }

  private def createPlan(valueMap: ValueMapT, response: Response, uploadText: String, subProcedureList: SubProcedureList): Unit = {
    Trace.trace("" + valueMap + response + subProcedureList) // gets rid of compiler warnings about unused parameters

    val doc = XML.loadString(uploadText.trim)

    /*
    val list = makeCheckedList(doc, subProcedureList)

    val beamList = {
      val l = list.map(_.beamList)
      val ii = l.indices

      def nameOf(i: Int) = list(i).beamList.map(_.beamName).sorted.mkString(" @@ ")

      def isRedundant(i: Int) = {
        val name = list(i).beamList.map(_.beamName).sorted.mkString(" @@ ")

        ???
      }

      val keep = ii.filterNot(isRedundant)

    }

    Trace.trace(beamList)
     */

    Trace.trace()
  }

  // true if the use clicked a checkbox
  private def updateBeamStatus(uploadText: String, response: Response, subProcedureList: SubProcedureList): Unit = {

    val doc = XML.loadString(uploadText.trim)

    val checkedBeamList = makeCheckedBeamList(doc, subProcedureList)

    val selList = subProcedureList.getSelectionsFromBeams(checkedBeamList)

    val js = Phase3JS.toJs(selList, checkedBeamList, subProcedureList)
    WebUtil.setResponse(js, response, Status.SUCCESS_OK)
  }

  private def buttonIs(valueMap: ValueMapT, button: IsInput): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val uploadText = {
      val t = request.getEntityAsText
      Option(t)
    }

    val valueMap = getValueMap(request)

    try {

      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach(): Unit =
        MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        case _ if buttonIs(valueMap, Phase3HtmlForm.cancelButton) =>
          updateMach()

        case _ if valueMap.contains(createPlanTag) =>
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
          createPlan(valueMap, response, uploadText.get, subProcedureList)

        case _ if request.getMethod == Method.POST =>
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
          updateBeamStatus(uploadText.get, response, subProcedureList)

        case _ => // first time viewing the form.  Set defaults
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
          formSelect(valueMap, response, subProcedureList, Seq())
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
