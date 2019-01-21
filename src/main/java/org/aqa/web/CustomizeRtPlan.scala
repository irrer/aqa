package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.CachedUser
import org.restlet.data.Status
import scala.xml.Elem
import org.aqa.Config
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import org.aqa.Util

object CustomizeRtPlan {
  private val path = new String((new CustomizeRtPlan).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlan extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

  def selectMachineName(response: Option[Response]): Seq[(String, String)] = {
    CachedUser.get(response.get.getRequest) match {
      case Some(user) => {
        val machList = Machine.listMachinesFromInstitution(user.institutionPK)
        machList.map(mach => (mach.machinePK.get.toString, mach.id))
      }
      case _ => Seq[(String, String)]()
    }
  }

  private def machineSelector = new WebInputSelect("Machine", true, 3, 0, selectMachineName, true)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match name in planning system", false)

  private def patientID = new WebInputText("Patient ID", true, 3, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private val row1 = List(machineSelector, toleranceTable)
  private val row2 = List(patientID, patientName)

  private lazy val planEnergyList: Seq[Double] = {
    val attrList = DicomUtil.findAll(Phase2Util.phase2Plan.attributeList.get, TagFromName.NominalBeamEnergy)
    val list = attrList.map(a => a.getDoubleValues.head).distinct.sorted
    logger.info("Energy list found in plan: " + list.mkString("  "))
    list
  }

  private def planEnergyOf(valueMap: ValueMapT): Elem = {
    ???
  }

  private def machEnergyOf(response: Option[Response]): Seq[(String, String)] = {
    ???
  }

  val energyRowList = {
    planEnergyList.indices.map(i => {
      val label = "Plan energy " + Util.fmtDbl(planEnergyList(i)) + " : "
      val energyPlan = new WebPlainText(label, false, 2, 0, (ValueMapT) => { <span>{ label }</span> })

    })
  }

  // Support up to 6 different energy levels in plan
  private val energyPlanA = new WebPlainText("Plan A", false, 2, 0, planEnergyOf)
  private val energyPlanB = new WebPlainText("Plan B", false, 2, 0, planEnergyOf)
  private val energyPlanC = new WebPlainText("Plan C", false, 2, 0, planEnergyOf)
  private val energyPlanD = new WebPlainText("Plan D", false, 2, 0, planEnergyOf)
  private val energyPlanE = new WebPlainText("Plan E", false, 2, 0, planEnergyOf)
  private val energyPlanF = new WebPlainText("Plan F", false, 2, 0, planEnergyOf)

  private val energyMachA = new WebInputSelect("Mach A", 2, 0, machEnergyOf)
  private val energyMachB = new WebInputSelect("Mach B", 2, 0, machEnergyOf)
  private val energyMachC = new WebInputSelect("Mach C", 2, 0, machEnergyOf)
  private val energyMachD = new WebInputSelect("Mach D", 2, 0, machEnergyOf)
  private val energyMachE = new WebInputSelect("Mach E", 2, 0, machEnergyOf)
  private val energyMachF = new WebInputSelect("Mach F", 2, 0, machEnergyOf)

  private val row3 = List(energyPlanA, energyMachA)
  private val row4 = List(energyPlanB, energyMachB)
  private val row5 = List(energyPlanC, energyMachC)
  private val row6 = List(energyPlanD, energyMachD)
  private val row7 = List(energyPlanE, energyMachE)
  private val row8 = List(energyPlanF, energyMachF)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val nextButton = makeButton("Next", true, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val selectButtonList: WebRow = List(nextButton, cancelButton)

  private val createButtonList: WebRow = List(nextButton, cancelButton)

  private def formSelect(valueMap: ValueMapT) = new WebForm(pathOf, List(row1, row2, selectButtonList))

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
