package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.FormButton

class Phase3TemplateNone extends Phase3Template {

  private val functionName = "TemplateNone"

  override def button: FormButton = makeButton("None", "Un-select everything.", s"$functionName()")

  override def js: String =
    s"""
       | function $functionName() {
       |   for (cb = 0; cb < checkboxList.length; cb++) {
       |     checkboxList[cb].checked = false;
       |   }
       |   phase3ClickHandler(null);
       | }
       |""".stripMargin
}
