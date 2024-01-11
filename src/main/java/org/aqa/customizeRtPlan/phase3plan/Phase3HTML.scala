package org.aqa.customizeRtPlan.phase3plan

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.customizeRtPlan.phase3plan.Phase3HtmlForm._
import org.aqa.db.Machine
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.Util
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
        val beamList2 = beamList.filterNot(beam => clicked.get._1.beamList.exists(b => b.beamName.equals(beam.beamName)))
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

      if (true) { // TODO rm
        val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
        val beamList = subProcedureList.subProcedureList.flatMap(_.selectionList.flatMap(_.beamList)).groupBy(_.beamName).values.map(_.head).toSeq.sortBy(_.beamName)

        val text = beamList.map(beam =>
          s"Beam Gantry: ${"%-24s".format(beam.beamName)}   Energy: ${"%4.1f".format(beam.beamEnergy.photonEnergy_MeV.get)} FFF: ${"%-4s"
            .format(beam.isFFF.toString)}    Gantry: ${beam.gantryAngleList_deg.map(Util.fmtDbl).mkString("    ")}"
        )
        Trace.trace("\n" + text.mkString("\n"))
      }

      if (true) { // TODO rm
        val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
        val beamList = subProcedureList.subProcedureList.flatMap(_.selectionList.flatMap(_.beamList)).groupBy(_.beamName).values.map(_.head).toSeq.sortBy(_.beamName)
        def colListOf(beam: Beam): Seq[Double] = {
          DicomUtil.findAllSingle(beam.prototypeBeam, TagByName.BeamLimitingDeviceAngle).flatMap(_.getDoubleValues)
        }

        val text = beamList.map(beam => s"Beam MLC: ${"%-24s".format(beam.beamName)} :: ${colListOf(beam).map(Util.fmtDbl).mkString("    ")}")
        Trace.trace("\n" + text.mkString("\n"))
      }

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
