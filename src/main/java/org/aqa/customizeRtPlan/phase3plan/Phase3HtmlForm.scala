package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Logging
import org.aqa.customizeRtPlan.CustomizeRtPlanInterface
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.data.Status
import org.restlet.Response

import scala.xml.Elem
import scala.xml.Node

object Phase3HtmlForm extends Logging {

  val pageTitle = "Select Tests for RTPLAN"

  /* HTML id of hidden input indicating which selection changed. */
  val changedSelectionTag = "changedSelection"

  val selectedBeamCountTag = "selectedBeamCount "


  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, Phase3HTML.subUrl, Phase3HTML.pathOf, buttonType)
  }

  def cancelButton: FormButton = makeButton("Cancel", ButtonType.BtnDefault)

  def createPlanButton: FormButton = makeButton("Create Plan", ButtonType.BtnPrimary)

  def machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private def patientID = new WebInputHidden(CustomizeRtPlanInterface.patientIdTag)

  private def patientName = new WebInputHidden(CustomizeRtPlanInterface.patientNameTag)

  private def machineName = new WebInputHidden(CustomizeRtPlanInterface.machineNameTag)

  private def planName = new WebInputHidden(CustomizeRtPlanInterface.planNameTag)

  private def toleranceTableName = new WebInputHidden(CustomizeRtPlanInterface.toleranceTableNameTag)


  /* Return the HTML attribute style for the given beam's use by a sub-procedure. */
  private def subProcBeamStyle(checked: Boolean): String = {
    val color =
      if (checked)
      //noinspection SpellCheckingInspection
        "lightgreen"
      else
        "white"
    val style = s"border-left: 16px solid $color; margin:5px;"
    style
  }


  private def makeSubProcedureSelector(subProcedureList: SubProcedureList): List[WebRow] = {
    def makeSelectorHtml(subProc: SubProcedure): WebRow = {
      val attrMap = Map("onClick" -> "phase3ClickHandler(this)")

      val list = subProc.selectionList.map(s => new WebInputCheckbox(label = s.selectionName, showLabel = true, title = None, col = 2, offset = 0, attrMap, id = Some(s.htmlId)))
      // @formatter:off
      val empty: Elem = { <span></span>}
      // @formatter:on
      val name = new WebPlainText(label = s"${subProc.name}:", showLabel = true, col = 12, offset = 0, _ => empty)
      (name +: list).toList
    }

    val checkBoxIdList: List[WebRow] = subProcedureList.subProcedureList.map(makeSelectorHtml).toList
    checkBoxIdList
  }

  /** A list of beam energies that the machine supports. If any have an undefined dose rate, then fill it in. */
  def rowList(subProcedureList: SubProcedureList): List[WebRow] = {

    val pageTitle = {
      val html =
        <div>
          <h2>Phase3 Custom Plan for Machine
            {WebUtil.wrapAlias(subProcedureList.metaData.machine.id)}
          </h2>
        </div>
      new WebPlainText(label = "Phase3 Plan", showLabel = false, col = 12, offset = 0, _ => html)
    }

    val rowTitle: WebRow = List(pageTitle)

    val rowPreDefined: WebRow = List(Phase3HtmlTemplate.uncheckAllButton())

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName)

    def rowSelectSubProcedures: List[WebRow] = makeSubProcedureSelector(subProcedureList)

    // @formatter:on
    def rowSelectedBeams: WebRow = List(Phase3HtmlBeam.selectedBeamsField(subProcedureList, selectedBeamCountTag))

    val rowButton: WebRow = List(Phase3HtmlForm.cancelButton, createPlanButton)

    val rowList: List[WebRow] = List(rowTitle) ++ List(rowPreDefined) ++ List(rowCommonParameters) ++ rowSelectSubProcedures ++ List(rowSelectedBeams) ++ List(rowButton)
    rowList
  }

  /**
   * Make a form to present to the user.
   *
   * @param valueMap         List of HTML field values.
   * @param response         HTML response.
   * @param subProcedureList Related information.
   */
  def formSelect(valueMap: ValueMapT, response: Response, subProcedureList: SubProcedureList): Unit = {
    val form = new WebForm(action = Phase3HTML.pathOf, title = None, rowList = rowList(subProcedureList), fileUpload = 0, runScript = Some(Phase3JS.javaScript)) // , runScript = Some(javaScript))

    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }


  private def toJs(checkedSelectionList: Seq[Selection], subProcedureList: SubProcedureList): String = {

    val falseList = subProcedureList.subProcedureList.flatMap(_.selectionList).filterNot(sel => checkedSelectionList.exists(_.selectionName.equals(sel.selectionName)))

    def selectionToJs(sel: Selection, checked: Boolean): String = {
      s""" document.getElementById("${sel.htmlId}").checked  = ${checked.toString}; """
    }

    val selectionText = (checkedSelectionList.map(sel => selectionToJs(sel, checked = true)) ++ falseList.map(sel => selectionToJs(sel, checked = false))).mkString("\n")


    def beamUseToJs(beam: Beam): String = {

      def beamSubProcToJs(subProc: SubProcedure): String = {
        val style = {
          // true if one of the
          val selForProc = checkedSelectionList.filter(sel => sel.subProcedure.name.equals(subProc.name))
          val checked = selForProc.flatMap(_.beamList.map(_.beamName)).contains(beam.beamName)
          subProcBeamStyle(checked)
        }
        s""" document.getElementById("${Phase3HtmlBeam.beamProcId(subProc, beam)}").style = "$style"; """
      }

      subProcedureList.subProcedureList.map(beamSubProcToJs).mkString("\n")
    }

    val beamText = subProcedureList.beamList.map(beamUseToJs).mkString("\n")

    val selectedBeamCountText = {
      val count = checkedSelectionList.flatMap(_.beamList).map(_.beamName).distinct.size.toString
      s""" document.getElementById("$selectedBeamCountTag").innerHTML = "$count"; """
    }


    Seq(selectionText, beamText, selectedBeamCountText).mkString("\n")
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
      if ((node \ "checked").text.toBoolean)
        subProcedureList.findByHtmlId((node \ "id").text)
      else
        None
    }

    (doc \ "Checkbox").flatMap(toSel)
  }


  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }


}
