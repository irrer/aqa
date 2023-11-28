package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil

case class Selection(subProcedure: SubProcedure, selectionName: String, beamList: Seq[Beam]) {

  /** Unique identifier in HTML for this selection. */
  val htmlId = s"${subProcedure.name}:${selectionName}".replaceAll("[^A-Za-z0-9_\\-:.]", "_")

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
