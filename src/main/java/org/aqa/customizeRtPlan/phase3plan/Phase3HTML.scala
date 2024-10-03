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

  private def createPlan(valueMap: ValueMapT, response: Response, subProcedureList: SubProcedureList): Unit = {
    Trace.trace("" + valueMap + response + subProcedureList) // gets rid of compiler warnings about unused parameters
    ???
  }

  /**
    * Given a node, return the selection ID and whether it is checked or not.
    *
    * @param node HTML description of checkbox.
    * @return True if checked.
    */
  private def toCheckbox(node: Node, subProcedureList: SubProcedureList): (Selection, Boolean) = {
    val id = (node \ "id").text
    val value = (node \ "checked").text.toBoolean
    (subProcedureList.findByHtmlId(id).get, value)
  }

  /**
    * Make a list of all the checked selections.
    *
    * @return List of checked selections.
    */
  private def makeCheckedList(doc: Elem, subProcedureList: SubProcedureList): Seq[Selection] = {

    def toSel(node: Node): Option[Selection] = {
      if ((node \ "checked").text.toBoolean) {
        val sel = subProcedureList.findByHtmlId((node \ "id").text)
        sel
      } else
        None
    }

    val list = (doc \ "Checkbox").flatMap(toSel)
    list
  }

  // true if the use clicked a checkbox
  private def updateBeamStatus(uploadText: Option[String], response: Response, subProcedureList: SubProcedureList): Unit = {

    val doc = XML.loadString(uploadText.get.trim)

    // if the user clicked a checkbox, then this is it with the new state
    val clicked = (doc \ "ClickedCheckbox").map(node => toCheckbox(node, subProcedureList)).headOption

    val init = makeCheckedList(doc, subProcedureList)

    val checkedList = if (clicked.isDefined) {
      // make list of beams that are selected due to checkboxes
      val beamList = init.flatMap(_.beamList).groupBy(_.beamName).map(_._2.head)

      if (clicked.get._2) { // changed from unchecked to checked
        // make a new list of selection based on the beams
        val selList = subProcedureList.getSelectionsFromBeams(beamList)
        selList
      } else { // changed from checked to unchecked
        // make a list of beam without the ones specified by newly unchecked one
        val sel = clicked.get._1
        // val keep = Set("M10G0C270", "M10G0C90", "Flood 6X")  // doing it this was is the lesser of two evils in terms of intuitiveness
        val selBeamNameList = sel.beamList.map(_.beamName) // .filterNot(n => keep.contains(n))
        val beamList2 = beamList.filterNot(beam => selBeamNameList.contains(beam.beamName))
        val selList = subProcedureList.getSelectionsFromBeams(beamList2)
        selList
      }
    } else {
      val beamList = init.flatMap(_.beamList).groupBy(_.beamName).map(_._2.head)
      val selList = subProcedureList.getSelectionsFromBeams(beamList)
      selList
    }

    val js = Phase3JS.toJs(checkedList, subProcedureList)
    // Trace.trace(checkboxList.mkString("\n"))
    // Trace.trace(js)

    WebUtil.setResponse(js, response, Status.SUCCESS_OK)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
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

        case _ if request.getMethod == Method.POST =>
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))

          updateBeamStatus(uploadText, response, subProcedureList)

        case _ if buttonIs(valueMap, Phase3HtmlForm.createPlanButton) =>
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
          createPlan(valueMap, response, subProcedureList)

        case _ => // first time viewing the form.  Set defaults
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
          formSelect(valueMap, response, subProcedureList)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
