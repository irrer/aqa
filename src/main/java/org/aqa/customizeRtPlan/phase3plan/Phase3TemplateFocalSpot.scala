package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.FormButton

class Phase3TemplateFocalSpot extends Phase3Template {

  private val functionName = "TemplateFocalSpot"

  override def button: FormButton = makeButton("Focal Spot", "Select all energies for focal spot and un-select the others.", s"$functionName()")

  private def htmlIdList(subProcedureList: SubProcedureList): Seq[String] = {
    val fs = subProcedureList.subProcedureList.find(_.isInstanceOf[SPFocalSpot]).get
    fs.selectionList.map(_.htmlId)
  }

  override def js(subProcedureList: SubProcedureList): String = {

    val idList = htmlIdList(subProcedureList)

    val booleanExpression = idList.map(id => s"""(id == "$id")""").mkString(" || ")

    s"""
       | function $functionName() {
       |   for (cb = 0; cb < checkboxList.length; cb++) {
       |     var id = checkboxList[cb].getAttribute("id");
       |     checkboxList[cb].checked = $booleanExpression;
       |   }
       |   phase3ClickHandler(null);
       | }
       |""".stripMargin
  }
}
