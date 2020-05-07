package org.aqa.run

import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine

object RunProcedure {

  def makeForm(runTrait: RunTrait) = {
    val machineSelector = new WebInputSelectMachine("Machine", 6, 0)

    def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
      val action = runTrait.procedureUrl + "?" + name + "=" + name
      new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    val runButton = makeButton("Run", true, ButtonType.BtnDefault)
    val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    val form = new WebForm(runTrait.procedureUrl, Some("BBbyEPID"), List(List(machineSelector), List(runButton, cancelButton)), 10)
    form
  }
}