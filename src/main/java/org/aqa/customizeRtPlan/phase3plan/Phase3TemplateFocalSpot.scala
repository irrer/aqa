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
