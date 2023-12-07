package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.FormButton

class Phase3TemplateFocalSpot extends Phase3Template {

  private val functionName = "TemplateFocalSpot"

  override def button: FormButton = makeButton("Focal Spot", "Select all energies for focal spot and un-select the others.", s"$functionName()")

  override def js: String =
    s"""
       | function $functionName() {
       |   for (cb = 0; cb < checkboxList.length; cb++) {
       |     var isFs = checkboxList[cb].getAttribute("id").startsWith("Focal Spot::");
       |     checkboxList[cb].checked = isFs;
       |   }
       |   phase3ClickHandler(null);
       | }
       |""".stripMargin
}
