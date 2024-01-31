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

import org.aqa.web.WebUtil.ButtonType
import org.aqa.web.WebUtil.FormButton
import org.aqa.Logging

/**
  * Base class for making pre-defined selection lists.  Supports things like an official RTPLAN for Phase3 research.
  */
abstract class Phase3Template extends Logging {

  /** Define a button that will be shown to the user. */
  def button: FormButton

  /** Generate the js that will change selections as needed. */
  def js(subProcedureList: SubProcedureList): String

  /**
    * Utility for creating a button appropriate for processing a template
    * @param label Name to be shown to user.
    * @param title Description of what the button does.
    * @param functionName JS function to be called when the button is clicked.
    * @return
    */
  final def makeButton(label: String, title: String, functionName: String) =
    new FormButton(
      label = label,
      col = 1,
      offset = 0,
      subUrl = Phase3HTML.subUrl,
      action = _ => Phase3HTML.pathOf,
      buttonType = ButtonType.BtnDefault,
      value = label,
      title = Some(title),
      htmlAttrMapP = Map("onclick" -> functionName),
      htmlButtonType = "button"
    )

}
