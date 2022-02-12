package org.aqa.simpleRtPlan

object SimpleRtplanJS {
  def runScript(beam: BeamInterface): String =
    s"""
       |
       |  // ------------------------------------------------------------------
       |
       |  function updateJaws(d1List, sumName, d1Name, d2Name) {
       |
       |    function addJaws(d1Elem) {
       |      var fullName = d1Elem.getAttribute("id");
       |      var baseName = fullName.substring(0, fullName.length - d1Name.length);
       |      var d2Elem = document.getElementById(baseName + d2Name);
       |      var sumElem = document.getElementById(baseName + sumName);
       |
       |      var d1 = parseFloat(d1Elem.value.trim());
       |      var d2 = parseFloat(d2Elem.value.trim());
       |      var sum = d1 + d2;
       |      sumElem.innerHTML = sum.toString();
       |      if (sum < 0) {
       |        sumElem.style.backgroundColor = "pink";
       |        sumElem.title = "Negative sized field not allowed.";
       |      }
       |      else {
       |        sumElem.style.backgroundColor = "white";
       |        sumElem.removeAttribute("title");
       |      }
       |    }
       |
       |    d1List.forEach(addJaws);
       |  }
       |
       |  // ------------------------------------------------------------------
       |
       |  var MUList = [];
       |
       |  function checkMU() {
       |    function grabMU(elem) {
       |      var id = elem.getAttribute("id");
       |      if (id.endsWith("${beam.labelMU}"))
       |        MUList.push(elem);
       |    }
       |
       |    if (MUList.length === 0)
       |      document.querySelectorAll("input").forEach(grabMU);
       |
       |    function checkOneMU(MUElem) {
       |      var muValue = parseFloat(MUElem.value.trim());
       |
       |      // MU is normal
       |      if (muValue < ${beam.MUWarnLimit}) {
       |        MUElem.style.backgroundColor = "white";
       |        MUElem.removeAttribute("title");
       |        return;
       |      }
       |
       |      // MU is excessive.
       |      if (muValue >= ${beam.MULimit}) {
       |        MUElem.style.backgroundColor = "pink";
       |        MUElem.title = "Error: MU is higher than the limit of ${beam.MULimit}";
       |        return;
       |      }
       |
       |      // warn user over use of high MU
       |      MUElem.style.backgroundColor = "yellow";
       |      MUElem.title = "Warning: MU is higher than the warning limit of ${beam.MUWarnLimit}";
       |    }
       |
       |    MUList.forEach(checkOneMU);
       |  }
       |
       |  // ------------------------------------------------------------------
       |
       |  var x1List = [];
       |  var y1List = [];
       |
       |  function grabX1(elem) {
       |    var id = elem.getAttribute("id");
       |    if ((id != null) && id.endsWith("${beam.labelX1}"))
       |      x1List.push(elem);
       |  }
       |
       |  function grabY1(elem) {
       |    var id = elem.getAttribute("id");
       |    if ((id != null) && id.endsWith("${beam.labelY1}"))
       |      y1List.push(elem);
       |  }
       |
       |  document.querySelectorAll("input").forEach(grabX1);
       |  document.querySelectorAll("input").forEach(grabY1);
       |
       |  // ------------------------------------------------------------------
       |
       |  var energyList = [];
       |
       |  function grabEnergy(elem) {
       |    var id = elem.getAttribute("id");
       |    if ((id != null) && id.endsWith("Energy")) {
       |      energyList.push(elem);
       |      if (elem.tagName.toLowerCase() === "select") {
       |        var action = "updateAllEnergy(this)";
       |        elem.setAttribute("onchange", action);
       |      }
       |    }
       |  }
       |
       |  function updateAllEnergy(elem) {
       |    var selection = elem.value;
       |    var text = elem.options[elem.selectedIndex].innerHTML;
       |
       |    for (i = 0; i < energyList.length; i++) {
       |      if (energyList[i].tagName.toLowerCase() === "select") {
       |        energyList[i].value = selection;
       |      }
       |      else {
       |        energyList[i].innerHTML = text;
       |      }
       |    }
       |  }
       |
       |  document.querySelectorAll("select").forEach(grabEnergy);
       |  document.querySelectorAll("input").forEach(grabEnergy);
       |  document.querySelectorAll("span").forEach(grabEnergy);
       |  document.querySelectorAll("div").forEach(grabEnergy);
       |
       |  // ------------------------------------------------------------------
       |
       |  var beamRefreshTime = 100;
       |
       |  function updateBeamLoop() {
       |    updateJaws(x1List, "${beam.labelFieldX}", "${beam.labelX1}", "${beam.labelX2}");
       |    updateJaws(y1List, "${beam.labelFieldY}", "${beam.labelY1}", "${beam.labelY2}");
       |    checkMU();
       |    setTimeout(updateBeamLoop, beamRefreshTime);
       |  }
       |
       |  updateBeamLoop();
       |
       |  // ------------------------------------------------------------------
       |
       |""".stripMargin
}
