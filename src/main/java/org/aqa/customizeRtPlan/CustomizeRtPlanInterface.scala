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

import org.aqa.Logging
import org.aqa.db.CachedUser
import org.aqa.db.Machine
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

  val machineIdTag = "MachineId"
  val patientIdTag = "Patient ID"
  val patientNameTag = "Patient Name"
  val machineNameTag = "Machine Name"
  val planNameTag = "Plan Name"
  val toleranceTableNameTag = "Tolerance Table Name"
}

/**
  * Generate a DICOM RTPLAN file customized for the user's environment.
  */
class CustomizeRtPlanInterface extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

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

  val machineId = new WebPlainText(CustomizeRtPlanInterface.machineIdTag, false, 6, 0, machineIdHtml)

  private def toleranceTable = new WebInputText(CustomizeRtPlanInterface.toleranceTableNameTag, true, 2, 0, "Should match name in planning system", false)

  private def planName = new WebInputText(CustomizeRtPlanInterface.planNameTag, true, 2, 0, "Name to distinguish this plan from others", false)

  /**
    * Name by which machine is referenced in DICOM files.  Often different than the common reference to the machine.
    *
    * @return Machine name.
    */
  private def machineName = {
    new WebInputText(CustomizeRtPlanInterface.machineNameTag, true, 2, 0, "To match planning system", false)
  }

  private def patientID = new WebInputText(CustomizeRtPlanInterface.patientIdTag, true, 3, 0, "")

  private def patientName = new WebInputText(CustomizeRtPlanInterface.patientNameTag, true, 3, 0, "")

  private val row0: WebRow = List(machineId)
  private val row2: WebRow = List(toleranceTable, planName, machineName)
  private val row1: WebRow = List(patientID, patientName)

  // List of all types of RTPLANS that can be made
  private val makeList = List(
    new MakeRtplanPhase2,
    new MakeRtplanPhase3,
    new MakeRtplanLOC,
    new MakeRtplanDailyQA,
    new MakeRtplanGapSkew,
    new MakeRtplanWinstonLutz,
    new MakeRtplanFocalSpot
  )

  class FormButtonProcedure(name: String, val procedure: Option[Procedure]) extends FormButton(name, col = 2, offset = 0, subUrl = subUrl, pathOf, ButtonType.BtnPrimary) {}

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def backButton = makeButton("Back", ButtonType.BtnDefault)

  private def rowList: List[WebRow] = {
    def parameterList: WebRow = List(row0, row1, row2)
    def procedureButtonList = makeList.map(_.makeButton)
    def cancelButtonList: WebRow = List(cancelButton, machinePK)
    List(
      parameterList,
      procedureButtonList.take(3),
      procedureButtonList.slice(3, 6),
      procedureButtonList.slice(6, 9),
      cancelButtonList
    )
  }

  private def makeForm = new WebForm(pathOf, rowList)

  private def formSelect(valueMap: ValueMapT, response: Response, machine: Machine): Unit = {
    val form = makeForm

    // if field is empty
    def empty(label: String) = !valueMap.contains(label) || valueMap(label).trim.isEmpty

    val defaultPatient = "$AQA_" + machine.machinePK.get

    def getRealMachineId = {
      machine.getRealTpsId match {
        case Some(text) if text.trim.nonEmpty => text.trim
        case _                                => ""
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

  private def showDownload(valueMap: ValueMapT, download: Download, response: Response): Unit = {
    val downloadUrl: String = WebServer.urlOfTmpFile(download.file)

    val downloadLink = new WebPlainText(
      "Download",
      false,
      3,
      0,
      _ => {
        <h4>
        <a href={downloadUrl} title="Click to download DICOM RTPLAN file(s).">Download</a>
      </h4>
      }
    )

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

      showDownload(valueMap, download, response)
    } else { // either user error or configuration error
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
        case _ if user.isEmpty                                                                                   => updateMach()
        case _ if machine.isEmpty                                                                                => updateMach()
        case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton)                                                               => updateMach()
        case _ if buttonIs(valueMap, backButton)                                                                 => updateMach()

        case _ if makeList.exists(make => valueMap.contains(make.makeButton.label)) => validateAndMake(valueMap, response)

        case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
