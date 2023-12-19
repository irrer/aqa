package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Logging
import org.aqa.customizeRtPlan.CustomizeRtPlanInterface
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil.WebPlainText
import org.restlet.data.Status
import org.restlet.Response

import scala.xml.Elem

object Phase3HtmlForm extends Logging {

  val pageTitle = "Select Tests for RTPLAN"

  /* HTML id of hidden input indicating which selection changed. */
  val changedSelectionTag = "changedSelection"

  val selectedBeamCountTag = "selectedBeamCount"

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

  /** List of pre-defined templates for making selections. */
  private def templateList: Seq[Phase3Template] = Seq(new Phase3TemplateNone, new Phase3TemplateAll, new Phase3TemplateFocalSpot)

  private def makeSubProcedureSelector(subProcedureList: SubProcedureList): Seq[Elem] = {
    def makeSelectorHtml(subProc: SubProcedure): WebRow = {
      val attrMap = Map("onClick" -> "phase3ClickHandler(this)")

      val list = subProc.selectionList.map(s => new WebInputCheckbox(label = s.selectionName, showLabel = true, title = None, col = 3, offset = 0, attrMap, id = Some(s.htmlId)))
      val name = new WebPlainText(label = s"${subProc.name}:", showLabel = false, col = 12, offset = 0, _ => <h3><hr/>{subProc.name}</h3>)
      (name +: list).toList
    }

    val checkBoxIdList = subProcedureList.subProcedureList.map(makeSelectorHtml).map(_.toHtml(emptyValueMap))
    checkBoxIdList
  }

  /**
    * HTML shown at top of web page.
    *
    * @param subProcedureList provide metadata.
    * @return HTML.
    */
  private def banner(subProcedureList: SubProcedureList): WebRow = {
    val pageTitle = {
      val html =
        <div>
          <h2>Phase3 Custom Plan for Machine
            {WebUtil.wrapAlias(subProcedureList.metaData.machine.id)}
          </h2>
        </div>
      new WebPlainText(label = "Phase3 Plan", showLabel = false, col = 12, offset = 0, _ => html)
    }

    List(pageTitle)
  }

  /** A list of beam energies that the machine supports. If any have an undefined dose rate, then fill it in. */
  def rowList(subProcedureList: SubProcedureList): List[WebRow] = {

    val rowTemplate: WebRow = templateList.map(_.button).toList

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName)

    def selectSubProceduresList = makeSubProcedureSelector(subProcedureList)

    def selectSubProcedureHtml = new WebPlainText(label = "Procedures", showLabel = false, col = 6, offset = 0, _ => <div>
      {selectSubProceduresList}
    </div>)

    // @formatter:on
    val beamHtml: WebPlainText = {
      val html = Phase3HtmlBeam.selectedBeamsField(subProcedureList, selectedBeamCountTag)
      val web = new WebPlainText(label = "Beams", showLabel = false, col = 6, offset = 0, _ => <div>
          {html}
        </div>)
      web
    }

    val row: WebRow = List(selectSubProcedureHtml, beamHtml)

    val rowButton: WebRow = List(Phase3HtmlForm.cancelButton, createPlanButton)

    val rowList: List[WebRow] = List(banner(subProcedureList)) ++ List(rowTemplate) ++ List(rowCommonParameters) ++ List(row) ++ List(rowButton)
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

    val templateText = Phase3HtmlForm.templateList.map(_.js).mkString("\n")
    val js = s"${Phase3JS.javaScript}\n$templateText"
    val form = new WebForm(action = Phase3HTML.pathOf, title = None, rowList = rowList(subProcedureList), fileUpload = 0, runScript = Some(js)) // , runScript = Some(javaScript))

    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

}
