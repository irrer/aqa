package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil

case class Selection (subProcedure: SubProcedure, selectionName: String, beamList: Seq[Beam]) {
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

