package org.aqa.customizeRtPlan.phase3plan

import org.aqa.web.WebUtil.ButtonType
import org.aqa.web.WebUtil.FormButton

object Phase3HtmlTemplate {

  def uncheckAllButton() =
    new FormButton(
      label = "Uncheck All",
      col = 2,
      offset = 0,
      subUrl = Phase3HTML.subUrl,
      action = _ => Phase3HTML.pathOf,
      buttonType = ButtonType.BtnDefault,
      value = "Uncheck All",
      title = Some("Uncheck all selections, making no beams selected."),
      htmlAttrMapP = Map("onclick" -> "uncheckAllFunction()"),
      htmlButtonType = "button"
    )

}
