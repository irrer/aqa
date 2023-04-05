/*
 * Copyright 2021 Regents of the University of Michigan
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

package org.aqa.customizeRtPlan

import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.db.MultileafCollimator
import org.aqa.db.Procedure
import org.aqa.web.MachineUpdate
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.Elem

object CustomizeRtPlanInterface {
  def reference(machinePK: Long): String = {
    (new CustomizeRtPlanInterface).pathOf + "?" + MachineUpdate.machinePKTag + "=" + machinePK
  }

  def redirect(machinePK: Long, response: Response): Unit = response.redirectSeeOther(reference(machinePK))

  def redirect(valueMap: ValueMapT, response: Response): Unit = redirect(valueMap(MachineUpdate.machinePKTag).toLong, response)

  val interface = new CustomizeRtPlanInterface

}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlanInterface extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

  private val machineIdTag = "MachineId"

  private val defaultPlanName = "AQA Plan"

  private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private def machineFromValueMap(valueMap: ValueMapT): Machine = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    machine
  }

  private def linkToMachineUpdate(valueMap: ValueMapT): Elem = {
    val machine = machineFromValueMap(valueMap)
    MachineUpdate.linkToMachineUpdate(machine.machinePK.get, machine.id)
  }

  private def machineIdHtml(valueMap: ValueMapT): Elem = {
    <h3 title="Plan customization for machine">Custom RT Plan Comptatible with Machine
      {linkToMachineUpdate(valueMap)}
    </h3>
  }

  val machineId = new WebPlainText(machineIdTag, false, 6, 0, machineIdHtml)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 2, 0, "Should match name in planning system", false)

  private def planName = new WebInputText("Plan Name", true, 2, 0, "Name to distinguish this plan from others", false)

  /**
   * Name by which machine is referenced in DICOM files.  Often different than the common reference to the machine.
   *
   * @return Machine name.
   */
  private def machineName = {
    new WebInputText("Machine Name", true, 2, 0, "To match planning system", false)
  }

  private def patientID = new WebInputText("Patient ID", true, 3, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private val row0: WebRow = List(machineId)
  private val row2: WebRow = List(toleranceTable, planName, machineName)
  private val row1: WebRow = List(patientID, patientName)

  private def getMachineEnergyList(machinePK: Long): Seq[MachineBeamEnergy] = {
    def compareMBE(a: MachineBeamEnergy, b: MachineBeamEnergy): Boolean = {

      val compare = (a.photonEnergy_MeV, b.photonEnergy_MeV, a.fffEnergy_MeV, b.fffEnergy_MeV) match {
        case (Some(aPho), Some(bPho), _, _) if aPho != bPho => aPho < bPho
        case (Some(_), _, _, _) => true
        case (_, Some(_), _, _) => false
        case (_, _, Some(aFFF), Some(bFFF)) if aFFF != bFFF => aFFF < bFFF
        case (_, _, Some(_), _) => true
        case (_, _, _, Some(_)) => false
        case _ => true
      }
      compare
    }

    val list = MachineBeamEnergy.getByMachine(machinePK).sortWith(compareMBE)
    list
  }

  // List of all types of RTPLANS that can be made
  private val makeList = Seq(
    new MakeRtplanFocalSpot, new MakeRtplanGapSkew, new MakeRtplanDailyQA, new MakeRtplanPhase2
  )

  // @formatter:off
  class FormButtonProcedure(
      name: String,
      val procedure: Option[Procedure],
      make: (Machine, Long, PlanSpecification, Seq[MachineBeamEnergy] , Option[Procedure] , Option[String] ) => Unit,
                           )

      extends FormButton(name, col = 2, offset = 0, subUrl = subUrl, pathOf, ButtonType.BtnPrimary) {}
  // @formatter:on

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def procedureButtonList = makeList.map(_.makeButton)


  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def backButton = makeButton("Back", ButtonType.BtnDefault)

  private def assignButtonList1: WebRow = procedureButtonList.toList

  private def assignButtonList2: WebRow = List(cancelButton, machinePK)

  private def makeForm = new WebForm(pathOf, List(row0, row1, row2) ++ List(assignButtonList1) ++ List(assignButtonList2))

  private def formSelect(valueMap: ValueMapT, response: Response, machine: Machine): Unit = {
    val form = makeForm

    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty

    val defaultPatient = "$AQA_" + machine.machinePK.get

    def getRealMachineId = {
      machine.getRealTpsId match {
        case Some(text) if text.trim.nonEmpty => text.trim
        case _ => ""
      }
    }

    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val machineNameMap = if (empty(machineName.label)) Map((machineName.label, getRealMachineId)) else emptyValueMap
    val planNameMap = if (empty(planName.label)) Map((planName.label, defaultPlanName)) else emptyValueMap

    val valMap = valueMap ++ patientIdMap ++ patientNameMap ++ machineNameMap ++ planNameMap
    form.setFormResponse(valMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def validateEntryFields(valueMap: ValueMapT): StyleMapT = {
    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty

    val tolErr = if (empty(toleranceTable.label)) Error.make(toleranceTable, "A tolerance table name must be given.") else styleNone
    val planNameErr = if (empty(planName.label)) Error.make(planName, "A plan name must be given.") else styleNone
    val machErr = if (empty(machineName.label)) Error.make(machineName, "A machine name must be given.") else styleNone
    val patIdErr = if (empty(patientID.label)) Error.make(patientID, "A patient ID must be given.") else styleNone
    val patNameErr = if (empty(patientName.label)) Error.make(patientName, "A patient name must be given.") else styleNone
    val patIdTooLongErr = if (valueMap(patientID.label).length > 64) Error.make(patientID, "Patient ID can not be over 64 characters..") else styleNone
    val planNameTooLongErr = if (valueMap(planName.label).length > 16) Error.make(patientID, "Plan Name can not be over 16 characters..") else styleNone
    // removed this check because it might stop people from generating a plan.  Technically the DICOM spec says that it
    // must be 16 characters or shorter, but the reality is that a lot of systems handle longer strings.
    //val machTooLongErr = if (valueMap(machineName.label).size > 16) Error.make(patientID, "Machine Name can not be over 16 characters..") else styleNone

    tolErr ++ planNameErr ++ machErr ++ patIdErr ++ patNameErr ++ patIdTooLongErr ++ planNameTooLongErr // ++ machTooLongErr
  }

  /**
   * Determine if there is a configuration entry for the given procedure and machine's collimator.
   */
  private def validateConfigured(planConfig: Option[Config.PlanFileConfig], machine: Machine, procName: String, button: FormButton) = {
    if (planConfig.isDefined) {
      styleNone
    } else {
      val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
      val msg = "The configuration does not contain an entry for machines with collimator type " + collimator.model + " for " + procName + "  Contact the system administrator."
      Error.make(button, msg)
    }
  }

  /**
   * Determine if there is an RTPLAN for the given procedure and machine's collimator.
   */
  private def validateRtplanFileExists(planConfig: Config.PlanFileConfig, procName: String, button: FormButton) = {
    if (planConfig.dicomFile.attributeList.isDefined) {
      styleNone
    } else {
      val msg = "There is a configuration entry for " + procName +
        " with collimator type " + planConfig.collimatorModel +
        " that references file " + planConfig.dicomFile.file.getAbsolutePath.replace('\\', '/') +
        " but that file does not exist. Contact the system administrator."
      Error.make(button, msg)
    }
  }

  private def validateConfigAndRtplanFileExists(planConfig: Option[Config.PlanFileConfig], machine: Machine, procName: String, button: FormButton) = {
    val exists = validateConfigured(planConfig, machine, procName, button)
    if (exists.isEmpty)
      validateRtplanFileExists(planConfig.get, procName, button)
    else
      exists
  }


  /**
   * Make sure fields are valid for LOC.
   * private def validateLOC(valueMap: ValueMapT): StyleMapT = {
   * val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
   * val locRtplan = CustomizeRtPlan.getCollimatorCompatibleLocPlanPairForMachine(machine)
   * val rtplanConfigErr = if (locRtplan.isEmpty) {
   * val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
   * Error.make(
   * createLocButton,
   * "Configuration error.  There is no configuration entry for LOC to support this machines with collimator " + collimator.model + " Contact the system administrator."
   * )
   * } else {
   * val rtplanConfigBaselineErr = validateConfigAndRtplanFileExists(Some(locRtplan.get.baseline), machine, "LOC Baseline", createLocButton)
   * val rtplanConfigDeliveryErr = validateConfigAndRtplanFileExists(Some(locRtplan.get.delivery), machine, "LOC Delivery", createLocButton)
   * rtplanConfigBaselineErr ++ rtplanConfigDeliveryErr
   * }
   *
   * validateEntryFields(valueMap) ++ rtplanConfigErr
   * }
   */

  private def showDownload(valueMap: ValueMapT, makeRtplan: MakeRtplan, download: Download, response: Response): Unit = {
    val downloadUrl: String = WebServer.urlOfTmpFile(download.file)

    val downloadLink = new WebPlainText("Download", false, 3, 0, _ => {
      <h4>
        <a href={downloadUrl} title="Click to download DICOM RTPLAN file(s).">Download</a>
      </h4>
    })


    val dicomView = new WebPlainText("Download", false, 10, 0, _ => download.elem)

    val r1: WebRow = List(downloadLink, backButton, machinePK)
    val r2: WebRow = List(dicomView)
    val form = new WebForm(pathOf, List(row0, r1, r2))

    form.setFormResponse(valueMap, styleNone, "Download RTPLAN", response, Status.SUCCESS_OK)
  }


  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  /**
   * Put user inputs into a class for use by the plan creator.
   */
  private def makePlanSpec(valueMap: ValueMapT) = {
    val planSpecification =
      PlanSpecification(valueMap(toleranceTable.label), valueMap(patientID.label), valueMap(patientName.label), valueMap(machineName.label), valueMap(planName.label))
    planSpecification
  }

  /**
   * Show the user a message saying why the creation of a custom rtplan failed.
   */
  private def showFailedCustomize(valueMap: ValueMapT, styleMap: StyleMapT, response: Response): Unit = {
    val form = makeForm
    form.setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
  }


  /**
   * If the form and configuration are valid, the make the RTPLAN(s).  If not, show error to user.
   *
   * @param valueMap Entered parameters.
   * @param response Write response here.
   */
  private def validateAndMake(valueMap: ValueMapT, response: Response): Unit = {
    val makeRtplan = makeList.find(pb => valueMap.contains(pb.makeButton.label)).get
    val styleMap = validateEntryFields(valueMap) ++ makeRtplan.validate(valueMap)

    if (styleMap.isEmpty) { // no errors - go ahead and make the RTPLAN(s)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get

      val userPk = WebUtil.getUser(valueMap).get.userPK.get

      val download = makeRtplan.showPlan(machine, userPk, makePlanSpec(valueMap), response)

      showDownload(valueMap, makeRtplan, download, response)
    }
    else { // either user error or configuration error
      val form = makeForm
      form.setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
    }

  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {
      val user = CachedUser.get(request)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach(): Unit =
        MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        case _ if user.isEmpty => updateMach()
        case _ if machine.isEmpty => updateMach()
        case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton) => updateMach()
        case _ if buttonIs(valueMap, backButton) => updateMach()

        case _ if procedureButtonList.exists(pb => valueMap.contains(pb.label)) => validateAndMake(valueMap, response)

        case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
