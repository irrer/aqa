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

  /* HTML id of hidden input indicating which selection changed. */
  private val changedSelectionTag = "changedSelection"

  private val selectedBeamCountTag = "selectedBeamCount "

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

  private def changedSelection = new WebInputHidden(changedSelectionTag)

  private val javaScript: String = {
    val machTag = MachineUpdate.machinePKTag

    //noinspection SpellCheckingInspection

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
       |var machinePK = document.getElementById("$machTag").getAttribute("value");
       |var phase3Url = "/Phase3HTML?$machTag=" + machinePK;
       |
       |function phase3ClickHandler(checkBox) {
       |  var text = "<?xml version='1.0' encoding='utf-8'?>\\n<CheckboxList>";
       |  if (checkBox != null) {
       |    text = text + "\\n  <ClickedCheckbox><id>" + checkBox.getAttribute("id") + "</id><checked>" + checkBox.checked.toString() + "</checked></ClickedCheckbox>";
       |  }
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
       |      var text = this.responseText;
       |      console.log("from the server:\\n" + text);
       |      eval(text);
       |    }
       |  }
       |
       |  console.log("Sending text\\n" + text);
       |  https.send(text);
       |  return text;
       |}
       |
       |populateCheckboxList();
       |
       |phase3ClickHandler(null);
       |
       |""".stripMargin
  }

  /**
   * Generate the HTML id for the given sub procedure's use of the given beam.
   *
   * @param subProc Sub-procedure.
   * @param beam    Beam.
   * @return HTML id.
   */
  private def beamProcId(subProc: SubProcedure, beam: Beam) = s"${subProc.name} :: ${beam.beamName}"

  /* Return true if the user has made any selections of the given sub procedure that use the given beam. */
  /*
  private def subProcIsUsingBeam(subProc: SubProcedure, beam: Beam, checkedSelectionList: Seq[Selection]): Boolean = {
    subProc.selectionList.exists(sel => checkedSelectionList.exists(s => s.htmlId.equals(sel.htmlId)))
  }
  */

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


  /** Return an indicator as to whether the sub procedure uses the beam. */
  private def subProcUseOfBeam(subProc: SubProcedure, beam: Beam): Elem = {
    val id = s"${subProc.name} :: ${beam.beamName}"
    <span id={id} style={subProcBeamStyle(false)} title={subProc.name}>
      <span style="margin:6px;">
        {subProc.name}
      </span>
    </span>
  }

  private def beamToHtml(beam: Beam, valueMap: ValueMapT, subProcedureList: SubProcedureList): Elem = {

    val beamSetUse = subProcedureList.subProcedureList.map(sub => subProcUseOfBeam(sub, beam))

    val subProcedureUses: Elem = {
      val usedBy = <td style="margin: 5px;">
        {beamSetUse.map(elem =>
          <div>
            {elem}
          </div>)}
      </td>
      usedBy
    }

    val image: Elem = {
      val imageUrl = subProcedureList.metaData.urlOfExampleImage(beam)
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
  private def selectedBeamsField(subProcedureList: SubProcedureList) = {
    def toBeamHtml(valueMap :ValueMapT): Elem = {
      <div>
        <div class="row">
          <div class="col-md-2">
            <h3>Beam List</h3>
          </div>
          <div class="col-md-2">
            <h3>Selected: <span id={selectedBeamCountTag}>0</span></h3>
          </div>
        </div>
        <div class="row">
          {subProcedureList.beamList.map(beam => {<div class="col-md-3"> {beamToHtml(beam, valueMap, subProcedureList)} </div>})}
        </div>
      </div>
    }

    new WebPlainText(
      label = "Selected Beams", showLabel = false,
      col = 0, offset = 0,
      toBeamHtml)
  }
  // @formatter:on

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
  private def rowList(subProcedureList: SubProcedureList): List[WebRow] = {

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

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName, changedSelection)

    def rowSelectSubProcedures: List[WebRow] = makeSubProcedureSelector(subProcedureList)

    def rowSelectedBeams: WebRow = List(selectedBeamsField(subProcedureList))

    val rowButton: WebRow = List(cancelButton, createPlanButton)

    val rowList: List[WebRow] = List(rowTitle) ++ List(rowCommonParameters) ++ rowSelectSubProcedures ++ List(rowSelectedBeams) ++ List(rowButton)
    rowList
  }

  /**
   * Make a form to present to the user.
   *
   * @param valueMap         List of HTML field values.
   * @param response         HTML response.
   * @param subProcedureList Related information.
   */
  private def formSelect(valueMap: ValueMapT, response: Response, subProcedureList: SubProcedureList): Unit = {
    val form = new WebForm(action = pathOf, title = None, rowList = rowList(subProcedureList), fileUpload = 0, runScript = Some(javaScript)) // , runScript = Some(javaScript))

    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

  private def createPlan(valueMap: ValueMapT, response: Response, subProcedureList: SubProcedureList): Unit = {
    Trace.trace("" + valueMap + response + subProcedureList) // gets rid of compiler warnings about unused parameters
    ???
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
        s""" document.getElementById("${beamProcId(subProc, beam)}").style = "$style"; """
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
      }
      else { // changed from checked to unchecked
        // make a list of beam without the ones specified by newly unchecked one
        val beamList2 = beamList.filterNot(beam => clicked.get._1.beamList.exists(b => b.beamName.equals(beam.beamName)))
        val selList = subProcedureList.getSelectionsFromBeams(beamList2)
        selList
      }
    }
    else
      init

    val js = toJs(checkedList, subProcedureList)
    // Trace.trace(checkboxList.mkString("\n"))
    Trace.trace(js)

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
        // case _ if CachedUser.get(request).isEmpty => updateMach()
        // case _ if machine.isEmpty => updateMach()
        // case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton) => updateMach()

        case _ if request.getMethod == Method.POST =>
          val subProcedureList = SubProcedureList.makeSubProcedureList(SPMetaData(machine.get))
          updateBeamStatus(uploadText, response, subProcedureList)


        case _ if buttonIs(valueMap, createPlanButton) =>
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
