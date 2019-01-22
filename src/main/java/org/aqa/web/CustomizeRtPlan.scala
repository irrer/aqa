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
import org.aqa.db.MultileafCollimator

object CustomizeRtPlan {
  val path = new String((new CustomizeRtPlan).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlan extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

  private val machineIdTag = "MachineId"

  private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private def machineIdHtml(valueMap: ValueMapT): Elem = {
    val machine = Machine.get(valueMap(machineIdTag).toLong).get
    <h4 aqaalias="">Machine: { machine.id }</h4>
  }

  val machineId = new WebPlainText(machineIdTag, false, 3, 0, machineIdHtml)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match name in planning system", false)

  private def patientID = new WebInputText("Patient ID", true, 3, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private val row1 = List(machineId, toleranceTable)
  private val row2 = List(patientID, patientName)

  private def getCollimatorCompatiblePlanForMachine(machine: Machine) = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFile = Config.Phase2PlanFileList.filter(pf => pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) && pf.model.equalsIgnoreCase(collimator.model)).head
    planFile
  }

  private def getPlanEnergyList(machine: Machine): Seq[Double] = {
    val planAttrList = getCollimatorCompatiblePlanForMachine(machine).dicomFile.attributeList.get
    val attrList = DicomUtil.findAll(planAttrList, TagFromName.NominalBeamEnergy)
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

  private def machEnergyTag(index: Int) = "MachEnergy" + index

  private def energyRowList(machine: Machine) = {
    val planEnergyList = getPlanEnergyList(machine)
    planEnergyList.indices.map(i => {
      val label = "Plan energy " + Util.fmtDbl(planEnergyList(i)) + " : "
      val energyPlan = new WebPlainText(label, false, 2, 0, (ValueMapT) => { <span>{ label }</span> })
      val energyMach = new WebInputSelect(machEnergyTag(i), false, 2, 0, machEnergyOf, false)
      val row = List(energyPlan, energyMach)
      row
    })
  }

  // Support up to 6 different energy levels in plan
  private val energyPlanA = new WebPlainText("Plan A", false, 2, 0, planEnergyOf)
  private val energyPlanB = new WebPlainText("Plan B", false, 2, 0, planEnergyOf)
  private val energyPlanC = new WebPlainText("Plan C", false, 2, 0, planEnergyOf)
  private val energyPlanD = new WebPlainText("Plan D", false, 2, 0, planEnergyOf)
  private val energyPlanE = new WebPlainText("Plan E", false, 2, 0, planEnergyOf)
  private val energyPlanF = new WebPlainText("Plan F", false, 2, 0, planEnergyOf)

  private val energyMachA = new WebInputSelect("Mach A", false, 2, 0, machEnergyOf, false)
  private val energyMachB = new WebInputSelect("Mach B", false, 2, 0, machEnergyOf, false)
  private val energyMachC = new WebInputSelect("Mach C", false, 2, 0, machEnergyOf, false)
  private val energyMachD = new WebInputSelect("Mach D", false, 2, 0, machEnergyOf, false)
  private val energyMachE = new WebInputSelect("Mach E", false, 2, 0, machEnergyOf, false)
  private val energyMachF = new WebInputSelect("Mach F", false, 2, 0, machEnergyOf, false)

  private val row3 = List(energyPlanA, energyMachA)
  private val row4 = List(energyPlanB, energyMachB)
  private val row5 = List(energyPlanC, energyMachC)
  private val row6 = List(energyPlanD, energyMachD)
  private val row7 = List(energyPlanE, energyMachE)
  private val row8 = List(energyPlanF, energyMachF)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", false, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(createButton, cancelButton)

  private def formSelect(valueMap: ValueMapT, response: Response) = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    energyRowList(machine)
    val form = new WebForm(pathOf, List(row1, row2, assignButtonList))
    form.setFormResponse(valueMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  //  private def emptyForm(valueMap: ValueMapT, response: Response): Unit = {
  //    formSelect(valueMap).setFormResponse(valueMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  //  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    val userId = WebUtil.getUserIdOrDefault(request, "guest")
    val user = WebUtil.getUser(request)
    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => MachineList.redirect(response)
        case _ if buttonIs(valueMap, createButton) => formSelect(valueMap, response)
        case _ => formSelect(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }
}
