package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.CachedUser
import org.restlet.data.Status

object CustomizeRtPlan {
  private val path = new String((new CustomizeRtPlan).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlan extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

  def collimatorName(response: Option[Response]): Seq[(String, String)] = {
    CachedUser.get(response.get.getRequest) match {
      case Some(user) => {
        val machList = Machine.listMachinesFromInstitution(user.institutionPK)
        machList.map(mach => (mach.machinePK.get.toString, mach.id))
      }
      case _ => Seq[(String, String)]()
    }
  }

  private def machineSelector = new WebInputSelect("Machine", 3, 0, collimatorName, true)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match name in planning system", true)

  private val selectFieldRow = List(machineSelector, toleranceTable)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val nextButton = makeButton("Next", true, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val selectButtonList: WebRow = List(nextButton, cancelButton)

  private val createButtonList: WebRow = List(nextButton, cancelButton)

  private def formSelect(valueMap: ValueMapT) = new WebForm(pathOf, List(selectFieldRow, selectButtonList))

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private def emptyForm(response: Response): Unit = {
    formSelect(emptyValueMap).setFormResponse(emptyValueMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    val userId = WebUtil.getUserIdOrDefault(request, "guest")
    val user = WebUtil.getUser(request)
    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => MachineList.redirect(response)
        case _ => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }
}
