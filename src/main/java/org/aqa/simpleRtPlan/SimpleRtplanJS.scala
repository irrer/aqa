package org.aqa.simpleRtPlan

object SimpleRtplanJS {
  def runScript(beam: BeamInterface): String =
    s"""
       |
       |    // ------------------------------------------------------------------
       |  
       |    var muName = "${beam.labelMU}";
       |    var doseRateName = "${beam.labelDoseRate}";
       |    var backupTimerName = "${beam.labelBackupTimer}";
       |  
       |    var energyName = "${beam.labelEnergy}";
       |  
       |    var x1Name = "${beam.labelX1}";
       |    var x2Name = "${beam.labelX2}";
       |    var fieldXName = "${beam.labelFieldX}";
       |  
       |    var y1Name = "${beam.labelY1}";
       |    var y2Name = "${beam.labelY2}";
       |    var fieldYName = "${beam.labelFieldY}";
       |
       |    var muWarnLimit = 400; // ${beam.MUWarnLimit};
       |    var muLimit = 1000; // ${beam.MULimit}
       |  
       |    // ------------------------------------------------------------------
       |  
       |    function updateJaws(d1List, sumName, d1Name, d2Name) {
       |  
       |      function addJaws(d1Elem) {
       |        var fullName = d1Elem.getAttribute("id");
       |        var baseName = fullName.substring(0, fullName.length - d1Name.length);
       |        var d2Elem = document.getElementById(baseName + d2Name);
       |        var sumElem = document.getElementById(baseName + sumName);
       |  
       |        var d1 = parseFloat(d1Elem.value.trim());
       |        var d2 = parseFloat(d2Elem.value.trim());
       |        var sum = d1 + d2;
       |        sumElem.innerHTML = sum.toString();
       |        if (sum < 0) {
       |          sumElem.style.backgroundColor = "pink";
       |          sumElem.title = "Negative sized field not allowed.";
       |        }
       |        else {
       |          sumElem.style.backgroundColor = "white";
       |          sumElem.removeAttribute("title");
       |        }
       |      }
       |  
       |      d1List.forEach(addJaws);
       |    }
       |  
       |    // ------------------------------------------------------------------
       |  
       |    function checkOneMU(MUElem) {
       |      var muValue = parseFloat(MUElem.value.trim());
       |  
       |      // MU is normal
       |      if (muValue < muWarnLimit) {
       |        MUElem.style.backgroundColor = "white";
       |        MUElem.removeAttribute("title");
       |      }
       |  
       |      // MU is excessive.
       |      if (muValue >= muLimit) {
       |        MUElem.style.backgroundColor = "pink";
       |        MUElem.title = "Error: MU is higher than the limit of " + muLimit;
       |      }
       |  
       |      // warn user over use of high MU
       |      if ((muValue >= muWarnLimit) && (muValue < muLimit)) {
       |        MUElem.style.backgroundColor = "yellow";
       |        MUElem.title = "Warning: MU is higher than the warning limit of " + muWarnLimit
       |      }
       |  
       |      // set the backup timer for this MU
       |      var fullLabel = MUElem.getAttribute("id");
       |      var baseLabel = fullLabel.substring(0, fullLabel.length - muName.length);
       |  
       |      var doseRateLabel = baseLabel + doseRateName;
       |      var doseRateElem = document.getElementById(doseRateLabel);
       |      var doseRate = parseFloat(doseRateElem.innerHTML);
       |  
       |      var backupTimeLabel = baseLabel + backupTimerName;
       |      var backupTimeElem = document.getElementById(backupTimeLabel);
       |  
       |      var mu = parseFloat(MUElem.value);
       |  
       |      var backupTime = (mu / doseRate) + 0.1;
       |      backupTimeElem.innerHTML = backupTime.toString().substring(0,6);
       |      console.log("backupTime: " + backupTime);
       |    }
       |
       |    function checkOneMUEvent(event) {
       |      checkOneMU(event.target);
       |    }
       |
       |    // Wait until after the page is loaded.  Otherwise filling in the initial values might trigger this.  TODO : check to see if this is true.
       |    setTimeout(function () {
       |      var list = document.querySelectorAll("input");
       |      var i = 0;
       |      for (i = 0; i < list.length; i++) {
       |        var elem = list[i];
       |        var id = elem.getAttribute("id");
       |        if (id.endsWith(muName)) {
       |          elem.addEventListener('keyup', checkOneMUEvent);
       |          checkOneMU(elem);
       |        }
       |      }
       |    }, 500);
       |  
       |    // ------------------------------------------------------------------
       |  
       |    var x1List = [];
       |    var y1List = [];
       |  
       |    function grabX1(elem) {
       |      var id = elem.getAttribute("id");
       |      if ((id != null) && id.endsWith(x1Name))
       |        x1List.push(elem);
       |    }
       |  
       |    function grabY1(elem) {
       |      var id = elem.getAttribute("id");
       |      if ((id != null) && id.endsWith(y1Name))
       |        y1List.push(elem);
       |    }
       |  
       |    document.querySelectorAll("input").forEach(grabX1);
       |    document.querySelectorAll("input").forEach(grabY1);
       |  
       |    // ------------------------------------------------------------------
       |  
       |    var energyList = [];
       |  
       |    function grabEnergy(elem) {
       |      var id = elem.getAttribute("id");
       |      if ((id != null) && id.endsWith(energyName)) {
       |        energyList.push(elem);
       |        if (elem.tagName.toLowerCase() === "select") {
       |          var action = "updateAllEnergy(this)";
       |          elem.setAttribute("onchange", action);
       |        }
       |      }
       |    }
       |  
       |    function updateAllEnergy(elem) {
       |      var selection = elem.value;
       |      var text = elem.options[elem.selectedIndex].innerHTML;
       |  
       |      var i = 0;
       |      for (i = 0; i < energyList.length; i++) {
       |        if (energyList[i].tagName.toLowerCase() === "select") {
       |          energyList[i].value = selection;
       |        }
       |        else {
       |          energyList[i].innerHTML = text;
       |        }
       |      }
       |    }
       |  
       |    document.querySelectorAll("select").forEach(grabEnergy);
       |    document.querySelectorAll("input").forEach(grabEnergy);
       |    document.querySelectorAll("span").forEach(grabEnergy);
       |    document.querySelectorAll("div").forEach(grabEnergy);
       |  
       |    // ------------------------------------------------------------------
       |  
       |    var beamRefreshTime = 100;
       |  
       |    function updateBeamLoop() {
       |      updateJaws(x1List, fieldXName, x1Name, x2Name);
       |      updateJaws(y1List, fieldYName, y1Name, y2Name);
       |      setTimeout(updateBeamLoop, beamRefreshTime);
       |    }
       |  
       |    var inputList = document.querySelectorAll("input");
       |    updateBeamLoop();
       |  
       |    // ------------------------------------------------------------------
       |  
       |""".stripMargin
}
