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
import org.aqa.customizeRtPlan.phase3plan.Phase3HtmlForm.selectedBeamCountTag
import org.aqa.web.MachineUpdate

/**
  * Javascript that handles Phase3 HTML.
  *
  * It responds to checkbox clicks and posts them to the server.  The response from
  * the server is javascript that it executes.
  */
object Phase3JS extends Logging {

  val colorOk = "white" // "lightgreen"
  private val colorCaution = "white" // "yellow"
  val colorInactive = "white"
  private val colorDisabled = "lightgrey"

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

  private def selectionToJs(sel: Selection, checked: Boolean): String = {
    s""" document.getElementById("${sel.htmlId}").checked = ${checked.toString}; """
  }

  private def selectionText(checkedSelectionList: Seq[Selection], subProcedureList: SubProcedureList): String = {
    val falseList = subProcedureList.subProcedureList.flatMap(_.selectionList).filterNot(sel => checkedSelectionList.exists(_.selectionName.equals(sel.selectionName)))
    (checkedSelectionList.map(sel => selectionToJs(sel, checked = true)) ++ falseList.map(sel => selectionToJs(sel, checked = false))).mkString("\n")
  }

  private def beamUseToJs(beam: Beam, checkedSelectionList: Seq[Selection], subProcedureList: SubProcedureList): String = {

    def beamSubProcToJs(subProc: SubProcedure): String = {
      val style = {
        // true if the sub processes reference the beam.
        val selForProc = checkedSelectionList.filter(sel => sel.subProcedure.name.equals(subProc.name))
        val checked = selForProc.flatMap(_.beamList.map(_.beamName)).contains(beam.beamName)
        Phase3HtmlBeam.subProcBeamStyle(checked)
      }
      s""" document.getElementById("${Phase3HtmlBeam.beamProcId(subProc, beam)}").style = "$style"; """
    }

    val showBeam: String = {
      val show = checkedSelectionList.exists(sel => sel.beamList.exists(b => b.beamName.equals(beam.beamName)))

      // the 'table-row' display type is the one that seems to work. Both 'block' and 'compact' make
      // the table row show up, but the columns of content is not aligned with the header columns.
      // see: https://www.w3schools.com/jsref/prop_style_display.asp
      val display = if (show) "table-row" else "none"
      s"""document.getElementById("${Phase3HtmlBeam.beamHtmlId(beam)}").style.display = "$display";"""
    }

    subProcedureList.subProcedureList.map(beamSubProcToJs).mkString("\n") + "\n" + showBeam
  }

  private def subProcessHeaderToJs(checkedSelectionList: Option[Seq[Selection]], subProcedure: SubProcedure): String = {

    val js = if (checkedSelectionList.nonEmpty) {
      //noinspection SpellCheckingInspection
      val beamCount = checkedSelectionList.get.flatMap(_.beamList.map(_.beamName)).distinct.size
      s"""document.getElementById("${subProcedure.headerId}").style.background = "$colorOk";""" +
        s"""document.getElementById("${subProcedure.headerId}").innerHTML = "${subProcedure.abbreviation + " : " + beamCount}";"""
    } else {
      s"""document.getElementById("${subProcedure.headerId}").style.background = "$colorDisabled";""" +
        s"""document.getElementById("${subProcedure.headerId}").innerHTML = "${subProcedure.abbreviation}";"""
    }

    js
  }

  private def gantryAngleUseToJs(checkedSelectionList: Seq[Selection]): String = {
    val angleList = checkedSelectionList.filter(_.subProcedure.usesCollimatorCentering).flatMap(_.beamList).filter(_.gantryAngleList_deg.size == 1).map(_.gantryAngle_roundedDeg).distinct.toSet

    def isCollimatorCentered(angle: Int): Boolean = {
      def isColCent(sel: Selection) = sel.subProcedure.isInstanceOf[SPCollimatorCentering]
      def hasAngle(sel: Selection) = sel.beamList.exists(_.gantryAngle_roundedDeg == angle)
      val is = checkedSelectionList.exists(sel => isColCent(sel) && hasAngle(sel))
      is
    }

    val isCollimatorCenteredMap: Map[Int, Boolean] = {
      val list = Seq(0, 90, 180, 270)
      val pairs = list.map(angle => (angle, isCollimatorCentered(angle)))
      pairs.toMap
    }

    def setAngleUse(angle: Int): String = {

      val color = (isCollimatorCenteredMap(angle), angleList.contains(angle)) match {
        case (true, _)     => colorOk
        case (false, true) => colorCaution
        case _             => colorInactive
      }

      s"""document.getElementById("${Phase3HtmlBeam.gantryAngleUsageId(angle)}").style.background = "$color";"""
    }

    val js = Seq(0, 90, 180, 270).map(setAngleUse).mkString("\n")
    js
  }

  /**
    * Set HTML highlighting to show which selections are covered by collimator centering.  Note that if the
    * procedure does not use collimator centering, then its selections are not highlighted.
    *
    * If a selection is selected, and uses collimator centering, but collimator centering is not being done
    * for a gantry angle that it uses, then it will be highlighted with colorCaution.
    *
    * @param checkedSelectionList Selections that are selected.
    * @param subProcedureList Metadata and sub-procedures.
    * @return js to set highlighting.
    */
  private def selectionGantryAngleUseToJs(checkedSelectionList: Seq[Selection], subProcedureList: SubProcedureList): String = {
    // List of gantry angles for which collimator centering is being done.
    val centeredAngleSet = checkedSelectionList.filter(_.subProcedure.isInstanceOf[SPCollimatorCentering]).flatMap(_.beamList).map(_.gantryAngle_roundedDeg).distinct

    def isCenteringForSelection(sel: Selection): Boolean = {
      val anglesRequired = sel.beamList.filter(_.gantryAngleList_deg.size == 1).map(_.gantryAngle_roundedDeg).distinct.sorted
      centeredAngleSet.intersect(anglesRequired).size == anglesRequired.size
    }

    def toJsStatement(selList: Seq[Selection], color: String) = {
      val body = selList.map(sel => s""" "${sel.htmlId}" """).mkString(",")
      val js = s"""setSelectionColor([ $body ], "$color");"""
      js
    }

    val okList = checkedSelectionList.filter(_.subProcedure.usesCollimatorCentering).filter(isCenteringForSelection)
    val cautionList = checkedSelectionList.filter(_.subProcedure.usesCollimatorCentering).filterNot(isCenteringForSelection)
    val uncheckedList = subProcedureList.subProcedureList.flatMap(_.selectionList).filterNot(sel => checkedSelectionList.exists(_.htmlId.equals(sel.htmlId)))

    Seq(
      toJsStatement(okList, colorOk),
      toJsStatement(cautionList, colorCaution),
      toJsStatement(uncheckedList, colorInactive)
    ).mkString("\n")

  }

  /**
    * Write js code that tells the client what to modify in the DOM.
    *
    * @param checkedSelectionList List of selections that the user has checked.
    * @param subProcedureList Sub-procedures and meta data related to this machine.
    * @return
    */
  def toJs(checkedSelectionList: Seq[Selection], subProcedureList: SubProcedureList): String = {

    val beamText = subProcedureList.beamList.map(beam => beamUseToJs(beam, checkedSelectionList, subProcedureList)).mkString("\n")

    val gantryAngleUseText = gantryAngleUseToJs(checkedSelectionList)

    val selText = selectionText(checkedSelectionList, subProcedureList)

    val selectedBeamCountText = {
      val count = checkedSelectionList.flatMap(_.beamList).map(_.beamName).distinct.size.toString
      s""" document.getElementById("$selectedBeamCountTag").innerHTML = "$count"; """
    }

    val checkedSubProcedureNameMap = checkedSelectionList.groupBy(_.subProcedure.name)
    val subHeaderText = subProcedureList.subProcedureList.map(sub => subProcessHeaderToJs(checkedSubProcedureNameMap.get(sub.name), sub)).mkString("\n")

    val js = Seq(selText, beamText, gantryAngleUseText, selectedBeamCountText, subHeaderText, selectionGantryAngleUseToJs(checkedSelectionList, subProcedureList)).mkString("\n")
    // Trace.trace(js)
    js
  }

}
