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

import org.aqa.web.WebUtil
import org.aqa.Util

case class Selection(subProcedure: SubProcedure, selectionName: String, beamList: Seq[Beam]) {

  /** Unique identifier in HTML for this selection. */
  val htmlId: String = Util.textToHtmlId(s"${subProcedure.name}::$selectionName")

  /**
    * Determine if the given ID refers to this selection.
    * @param id Test this id.
    * @return True if it matches.
    */
  def htmlIdMatches(id: String): Boolean = id.equals(htmlId)

  /**
    * Determine if this selection has been selected by the user.
    *
    * @param valueMap User selections.
    * @return True if selected.
    */
  def isSelected(valueMap: WebUtil.ValueMapT): Boolean = {
    valueMap.contains(selectionName)
  }
}
