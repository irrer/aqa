package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Logging
import org.aqa.web.MachineUpdate

/**
  * Javascript that handles Phase3 HTML.
  *
  * It responds to checkbox clicks and posts them to the server.  The response from
  * the server is javascript that it executes.
  */
object Phase3JS extends Logging {

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

}
