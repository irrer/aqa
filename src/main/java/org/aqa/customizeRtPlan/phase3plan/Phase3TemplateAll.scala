package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.FormButton

class Phase3TemplateAll extends Phase3Template {

  private val functionName = "TemplateAll"

  override def button: FormButton = makeButton("All", "Select everything.", s"$functionName()")

  override def js(subProcedureList: SubProcedureList): String =
    s"""
       | function $functionName() {
       |   for (cb = 0; cb < checkboxList.length; cb++) {
       |     checkboxList[cb].checked = true;
       |   }
       |   phase3ClickHandler(null);
       | }
       |""".stripMargin
}
