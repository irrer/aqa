package org.aqa.customizeRtPlan.phase3plan

import edu.umro.ScalaUtil.Trace
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.customizeRtPlan.CustomizeRtPlanInterface
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

class Phase3HTML extends Restlet with SubUrlRoot with Logging {

  private val pageTitle = "Select Tests for RTPLAN"


  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def createPlanButton = makeButton("Create Plan", ButtonType.BtnPrimary)

  private def machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private def patientID = new WebInputHidden(CustomizeRtPlanInterface.patientIdTag)

  private def patientName = new WebInputHidden(CustomizeRtPlanInterface.patientNameTag)

  private def machineName = new WebInputHidden(CustomizeRtPlanInterface.machineNameTag)

  private def planName = new WebInputHidden(CustomizeRtPlanInterface.planNameTag)

  private def toleranceTableName = new WebInputHidden(CustomizeRtPlanInterface.toleranceTableNameTag)

  private val javaScript: String = {
    val machTag = MachineUpdate.machinePKTag
    s"""
       |
       |// handle Phase3 custom plan checkboxes and beam status indications.
       |
       |var checkboxList = [];
       |
       |function populateCheckboxList() {
       |  checkboxList.length = 0;  // make sure list is empty
       |  var publicList = $$( "[type]" );
       |  for (p = 0; p < publicList.length; p++) {
       |    var attr = publicList[p];
       |    // is this a checkbox?
       |    if (
       |      (attr.tagName.toLowerCase().localeCompare("input") == 0)                 &&
       |      attr.hasAttribute("type")                                                &&
       |      (attr.getAttribute("type").toLowerCase().localeCompare("checkbox") == 0)
       |      ) {
       |        checkboxList.push(attr);
       |    }
       |  }
       |}
       |
       |console.log("list of checkboxes:\\n" + checkboxList);
       |
       |var machinePK = 27; // document.getElementById("$machTag").getAttribute("value");
       |var phase3Url = "/Phase3HTML?$machTag=" + machinePK;
       |
       |function phase3ClickHandler(checkBox) {
       |  var text = "<?xml version='1.0' encoding='utf-8'?>\\n<CheckboxList>";
       |  for (c = 0; c < checkboxList.length; c++) {
       |    attr = checkboxList[c];
       |    var id = attr.getAttribute("id");
       |    var checked = attr.checked.toString();
       |    text = text + "\\n  <Checkbox><id>" + id + "</id><checked>" + checked + "</checked></Checkbox>";
       |  }
       |  text = text + "\\n</CheckboxList>";
       |  var https = new XMLHttpRequest();
       |  https.open("POST", phase3Url, true);
       |  https.setRequestHeader('Content-type', '${org.restlet.data.MediaType.TEXT_PLAIN.getName}');
       |
       |  https.onreadystatechange = function() {
       |    if (this.readyState == 4 && this.status == 200) {
       |      console.log("from server:\\n" + this.responseText);
       |    }
       |  }
       |
       |  console.log("Sending text\\n" + text);
       |  https.send(text);
       |  return text;
       |}
       |
       |populateCheckboxList()
       |
       |""".stripMargin
  }


  /** Return an indicator as to whether the sub procedure uses the beam. */
  private def subProcUseOfBeam(subProc: SubProcedure, beam: Beam, valueMap: ValueMapT): Elem = {
    val color =
    //if (subProc.metaData.subProcedureUsesBeam(beam, subProc, valueMap)) // TODO put back
      if (beam.isFFF) // TODO rm
      //noinspection SpellCheckingInspection
        "lightgreen"
      else
        "white"

    val id = s"${subProc.name} :: ${beam.beamName}"
    <span id={id} style={s"border: 5px solid $color;border-radius:40px; margin:5px;"} title={subProc.name}>
      <span style="margin:6px;">
        {subProc.abbreviation}
      </span>
    </span>

  }


  private def beamToHtml(beam: Beam, valueMap: ValueMapT, metaData: SPMetaData): Elem = {

    val beamSetUse = metaData.subProcedureList.map(sub => subProcUseOfBeam(sub, beam, valueMap))

    val subProcedureUses: Elem = {
      val usedBy = <td style="margin: 5px;">
        {beamSetUse}
      </td>
      usedBy
    }

    val image: Elem = {
      val imageUrl = metaData.urlOfExampleImage(beam)
      <div style="margin: 5px;">
        <img src={imageUrl} height="100" style="margin: 2px;"/>
        <br/>{beam.beamName}
      </div>
    }

    <div style="border:1px solid lightgrey;margin: 2px;">
      <table>
        <tr>
          <td>
            {image}
          </td>
          <td>
            {subProcedureUses}
          </td>
        </tr>
      </table>
    </div>
  }

  // @formatter:off
  private def selectedBeamsField(metaData: SPMetaData) = {
    new WebPlainText(
      label = "Selected Beams", showLabel = false,
      col = 0, offset = 0,
      // valueMap => <span> {list.map(beamName => {<br>{beamName}</br>})} </span>)
      // _ => <span> {list.map(beamName => {<span style="margin-left: 32px;">{beamName}</span>})} </span>)
    valueMap => <div> {metaData.prototypeBeamList.map(beam => {<div class="col-md-3"> {beamToHtml(beam, valueMap, metaData)} </div>})} </div>)
  }
  // @formatter:on

  private def makeSubProcedureSelector(metaData: SPMetaData): List[WebRow] = {
    def makeSelectorHtml(subProc: SubProcedure): WebRow = {
      val attrMap = Map("onClick" -> "phase3ClickHandler(this)")
      val list = subProc.selectionList.map(s => new WebInputCheckbox(label = s.selectionName, showLabel = true, title = None, col = 2, offset = 0, attrMap))
      // @formatter:off
      val empty: Elem = { <span></span>}
      // @formatter:on
      val name = new WebPlainText(label = s"${subProc.name}:", showLabel = true, col = 12, offset = 0, _ => empty)
      (name +: list).toList
    }

    val checkBoxIdList: List[WebRow] = metaData.subProcedureList.map(makeSelectorHtml).toList
    checkBoxIdList
  }

  /** A list of beam energies that the machine supports. If any have an undefined dose rate, then fill it in. */
  private def rowList(metaData: SPMetaData): List[WebRow] = {

    val pageTitle = {
      val html =
        <div>
          <h2>Phase3 Custom Plan for Machine
            {WebUtil.wrapAlias(metaData.machine.id)}
          </h2>
        </div>
      new WebPlainText(label = "Phase3 Plan", showLabel = false, col = 12, offset = 0, _ => html)
    }

    val rowTitle: WebRow = List(pageTitle)

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName)

    def rowSelectSubProcedures: List[WebRow] = makeSubProcedureSelector(metaData)

    def rowSelectedBeams: WebRow = List(selectedBeamsField(metaData))

    val rowButton: WebRow = List(cancelButton, createPlanButton)

    val rowList: List[WebRow] = List(rowTitle) ++ List(rowCommonParameters) ++ rowSelectSubProcedures ++ List(rowSelectedBeams) ++ List(rowButton)
    rowList
  }

  /**
   * Make a form to present to the user.
   *
   * @param valueMap List of HTML field values.
   * @param response HTML response.
   * @param metaData Related information.
   */
  private def formSelect(valueMap: ValueMapT, response: Response, metaData: SPMetaData): Unit = {
    val form = new WebForm(action = pathOf, title = None, rowList = rowList(metaData), fileUpload = 0, runScript = Some(javaScript)) // , runScript = Some(javaScript))

    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

  private def createPlan(valueMap: ValueMapT, response: Response, metaData: SPMetaData): Unit = {
    Trace.trace()
    ???
  }

  private def updateBeamStatus(valueMap: WebUtil.ValueMapT, uploadText: Option[String], response: Response): Unit = {

    val doc = XML.loadString(uploadText.get.trim)

    val checkboxList = {

      def toCheckbox(node: Node): (String, Boolean) = {
        val id = (node \ "id").text
        val value = (node \ "checked").text.toBoolean
        (id, value)
      }

      val map = (doc \ "Checkbox").map(toCheckbox).toMap
      map
    }

    Trace.trace(checkboxList)

    val text = "hello " + new java.util.Date() // TODO
    WebUtil.setResponse(text, response, Status.SUCCESS_OK)
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
        // case _ if CachedUser.get(request).isEmpty => updateMach()
        // case _ if machine.isEmpty => updateMach()
        // case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton) => updateMach()
        case _ if request.getMethod == Method.POST =>
          updateBeamStatus(valueMap, uploadText, response)
        case _ if buttonIs(valueMap, createPlanButton) =>
          val metaData = SPMetaData(machine.get)
          createPlan(valueMap, response, metaData)

        case _ => // first time viewing the form.  Set defaults
          val metaData = SPMetaData(machine.get)
          formSelect(valueMap, response, metaData)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
