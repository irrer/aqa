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

import org.aqa.Logging
import org.aqa.web.MachineUpdate

/**
  * Javascript that handles Phase3 HTML.
  *
  * It responds to checkbox clicks and posts them to the server.  The response from
  * the server is javascript that it executes.
  */
object Phase3JS extends Logging {

  val colorBackground = "white"
  val colorSelected = "lightblue"
  private val colorCaution = "white" // "yellow"
  val colorInactive = "white"

  /* HTML id of hidden input indicating which selection changed. */
  val changedSelectionTag = "changedSelection"

  val createPlanTag = "createPlan"

  val selectedBeamCountTag = "selectedBeamCount"

  val javaScript: String = {
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
       |//console.log("list of checkboxes:\\n" + checkboxList);
       |
       |var machinePK = document.getElementById("$machTag").getAttribute("value");
       |var phase3Url = "/Phase3HTML?$machTag=" + machinePK;
       |
       |var createPlan = "$createPlanTag";
       |
       |function beamName(name) {
       |  var i = name.indexOf("::");
       |  var s = name.substring(0, i);
       |  return s;
       |}
       |
       |function checkOtherCheckboxes(name, state) {
       |  for (c = 0; c < checkboxList.length; c++) {
       |    attr = checkboxList[c];
       |    var id = beamName(attr.getAttribute("id"));
       |    if (id == name) {
       |      attr.checked = state;
       |    }
       |  }
       |}
       |
       |function setSubProcedure() {
       |  var i = 0;
       |  var subProcedureList = $$( "[subProcedure]" );
       |
       |  for (i = 0; i < subProcedureList.length; i++) {
       |    var inputList = subProcedureList[i].getElementsByTagName("input");
       |    var allTrue = true;
       |    for (ii = 0; ii < inputList.length; ii++) {
       |       var j = inputList[ii].state;
       |       allTrue = allTrue && inputList[ii].checked;
       |    }
       |
       |    var color = "$colorBackground";
       |    if (allTrue) {
       |      color = "$colorSelected";
       |    }
       |    subProcedureList[i].style.background = color;
       |  }
       |}
       |
       |function phase3ClickHandler(checkBox) {
       |  var text = "<?xml version='1.0' encoding='utf-8'?>\\n<CheckboxList>";
       |  var postUrl = phase3Url;
       |  if ((checkBox != null) && (checkBox != createPlan)) {
       |    var name = beamName(checkBox.getAttribute("id"));
       |    text = text + "\\n  <ClickedCheckbox><id>" + name + "</id><checked>" + checkBox.checked.toString() + "</checked></ClickedCheckbox>";
       |    checkOtherCheckboxes(name, checkBox.checked);
       |  }
       |
       |  setSubProcedure();
       |
       |  if ((checkBox != null) && (checkBox == createPlan)) {
       |    postUrl = postUrl + "&" + createPlan + "=true";
       |  }
       |
       |  var uniqueSet = new Set();
       |  for (c = 0; c < checkboxList.length; c++) {
       |    attr = checkboxList[c];
       |    var id = beamName(attr.getAttribute("id"));
       |    var checked = attr.checked.toString();
       |    var item = "\\n  <Checkbox><id>" + id + "</id><checked>" + checked + "</checked></Checkbox>";
       |    uniqueSet.add(item);
       |  }
       |
       |  var uniqueArray = Array.from(uniqueSet);
       |  for (c = 0; c < uniqueArray.length; c++) {
       |    text = text + uniqueArray[c];
       |  }
       |
       |  text = text + "\\n</CheckboxList>";
       |  console.log(text);
       |  var https = new XMLHttpRequest();
       |  https.open("POST", postUrl, true);
       |  https.setRequestHeader('Content-type', '${org.restlet.data.MediaType.TEXT_PLAIN.getName}');
       |
       |  https.onreadystatechange = function() {
       |    if (this.readyState == 4 && this.status == 200) {
       |      var text = this.responseText;
       |      // console.log("from the server:\\n" + text);
       |      eval(text);
       |    }
       |  }
       |
       |  //console.log("Sending text\\n" + text);
       |  https.send(text);
       |  return text;
       |}
       |
       |populateCheckboxList();
       |
       |phase3ClickHandler(null);
       |
       |
       |function setSelectionColor(list, color) {
       |  for (i = 0; i < list.length; i++) {
       |    var elem = document.getElementById(list[i]).parentElement.parentElement;
       |    var styl = "border-left: 5px solid " + color + "; border-bottom: 4px solid " + color + "; background-color: " + color;
       |    elem.style = styl;
       |  }
       |}
       |
       |""".stripMargin
  }

  private def selectionText(checkedBeamList: Seq[Beam], subProcedureList: SubProcedureList): String = {

    def toJs(subProcedure: SubProcedure): String = {
      val color =
        if (subProcedure.subIsSelected(checkedBeamList))
          colorSelected
        else
          colorBackground

      val js = s"""document.getElementById("${subProcedure.headerId}").style.background = "$color";"""
      js
    }

    val text = subProcedureList.subProcedureList.map(toJs).mkString("\n")

    text
  }

  /**
    * Set the visibility of each beam to visible or not.
    * @param checkedBeamList List of beams that user has checked.
    * @param subProcedureList Contains list of all beams.
    * @return
    */
  private def beamUseToJs(checkedBeamList: Seq[Beam], subProcedureList: SubProcedureList): String = {

    def setBeamVisibility(beam: Beam): String = {
      val state =
        if (checkedBeamList.exists(b => b.beamName.equals(beam.beamName)))
          "table-row"
        else
          "none"
      val text = s"""document.getElementById("${beam.beamName}").style.display = "$state";"""
      text
    }

    val jsText = subProcedureList.allBeams.map(setBeamVisibility).mkString("\n") + "\n"

    jsText
  }

  private def subProcessHeaderToJs(checkedSelectionList: Option[Seq[Selection]], subProcedure: SubProcedure): String = {

    val js = if (checkedSelectionList.nonEmpty) {
      //noinspection SpellCheckingInspection
      val beamCount = checkedSelectionList.get.flatMap(_.beamList.map(_.beamName)).distinct.size
      s"""document.getElementById("${subProcedure.headerId}").style.background = "$colorSelected";""" +
        s"""document.getElementById("${subProcedure.headerId}").innerHTML = "${subProcedure.abbreviation + " <br/> " + beamCount}";"""
    } else {
      s"""document.getElementById("${subProcedure.headerId}").style.background = "$colorBackground";""" +
        s"""document.getElementById("${subProcedure.headerId}").innerHTML = "${subProcedure.abbreviation + " <p> </p> "}";"""
    }

    js
  }

  /**
    * Write js code that tells the client what to modify in the DOM.
    *
    * @param checkedSelectionList List of selections that the user has checked.
    * @param subProcedureList Sub-procedures and meta data related to this machine.
    * @return
    */
  def toJs(checkedSelectionList: Seq[Selection], checkedBeamList: Seq[Beam], subProcedureList: SubProcedureList): String = {

    val beamText = beamUseToJs(checkedBeamList, subProcedureList)

    val selText = selectionText(checkedBeamList, subProcedureList)

    val selectedBeamCountText = s""" document.getElementById("${Phase3JS.selectedBeamCountTag}").innerHTML = "${checkedBeamList.size}"; """

    val checkedSubProcedureNameMap = checkedSelectionList.groupBy(_.subProcedure.name)
    val subHeaderText = subProcedureList.subProcedureList.map(sub => subProcessHeaderToJs(checkedSubProcedureNameMap.get(sub.name), sub)).mkString("\n")

    val beamUsageBySubProcedure = Phase3HtmlBeam.setBeamUsageBySubProcedure(subProcedureList, checkedBeamList)

    val js = Seq(selText, beamText, selectedBeamCountText, subHeaderText, beamUsageBySubProcedure).mkString("\n")
    js
  }

}
