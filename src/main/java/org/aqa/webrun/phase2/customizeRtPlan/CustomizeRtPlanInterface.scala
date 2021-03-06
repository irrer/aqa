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

package org.aqa.webrun.phase2.customizeRtPlan

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CachedUser
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.db.MultileafCollimator
import org.aqa.web.MachineUpdate
import org.aqa.web.WebServer
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.io.File
import java.util.Date
import scala.xml.Elem

object CustomizeRtPlanInterface {
  def reference(machinePK: Long): String = { (new CustomizeRtPlanInterface).pathOf + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  def redirect(machinePK: Long, response: Response): Unit = response.redirectSeeOther(reference(machinePK))

  def redirect(valueMap: ValueMapT, response: Response): Unit = redirect(valueMap(MachineUpdate.machinePKTag).toLong, response)
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
        case (Some(_), _, _, _)                             => true
        case (_, Some(_), _, _)                             => false
        case (_, _, Some(aFFF), Some(bFFF)) if aFFF != bFFF => aFFF < bFFF
        case (_, _, Some(_), _)                             => true
        case (_, _, _, Some(_))                             => false
        case _                                              => true
      }
      compare
    }

    val list = MachineBeamEnergy.getByMachine(machinePK).sortWith(compareMBE)
    list
  }

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private val createPhase2Button = makeButton("Create Phase 2", ButtonType.BtnPrimary)
  private val createPhase3Button = makeButton("Create Phase 3", ButtonType.BtnPrimary)
  private val createLocButton = makeButton("Create LOC", ButtonType.BtnPrimary)
  private val createDailyQAButton = makeButton("Create Daily QA", ButtonType.BtnPrimary)
  private val createGapSkewButton = makeButton("Create Gap Skew", ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", ButtonType.BtnDefault)
  private val backButton = makeButton("Back", ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(createPhase2Button, createPhase3Button, createLocButton, createDailyQAButton, createGapSkewButton, cancelButton, machinePK)

  private def formSelect(valueMap: ValueMapT, response: Response, machine: Machine): Unit = {
    val form = new WebForm(pathOf, List(row0, row1, row2) ++ List(assignButtonList))

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
    * Make sure fields are valid for Phase 2.
    */
  private def validatePhase2(valueMap: ValueMapT): StyleMapT = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val phase2Rtplan = CustomizeRtPlan.getCollimatorCompatiblePhase2PlanForMachine(machine)
    val conf = validateConfigAndRtplanFileExists(phase2Rtplan, machine, "Phase 2", createPhase2Button)
    validateEntryFields(valueMap) ++ conf
  }

  /**
    * Make sure fields are valid for Phase 3.
    */
  private def validatePhase3(valueMap: ValueMapT): StyleMapT = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val phase3Rtplan = CustomizeRtPlan.getCollimatorCompatiblePhase3PlanForMachine(machine)
    val conf = validateConfigAndRtplanFileExists(phase3Rtplan, machine, "Phase 3", createPhase2Button)
    validateEntryFields(valueMap) ++ conf
  }

  /**
    * Make sure fields are valid for DailyQA.
    */
  private def validateDailyQA(valueMap: ValueMapT): StyleMapT = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val dailyQaRtplan = CustomizeRtPlan.getCollimatorCompatibleDailyQAPlanForMachine(machine)
    val conf = validateConfigAndRtplanFileExists(dailyQaRtplan, machine, "Daily QA", createDailyQAButton)
    validateEntryFields(valueMap) ++ conf
  }

  /**
    * Make sure fields are valid for DailyQA.
    */
  private def validateGapSkew(valueMap: ValueMapT): StyleMapT = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val gapSkewRtplan = CustomizeRtPlan.getCollimatorCompatibleGapSkewPlanForMachine(machine)
    val conf = validateConfigAndRtplanFileExists(gapSkewRtplan, machine, "Gap Skew", createGapSkewButton)
    validateEntryFields(valueMap) ++ conf
  }

  /**
    * Make sure fields are valid for LOC.
    */
  private def validateLOC(valueMap: ValueMapT): StyleMapT = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val locRtplan = CustomizeRtPlan.getCollimatorCompatibleLocPlanPairForMachine(machine)
    val rtplanConfigErr = if (locRtplan.isEmpty) {
      val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
      Error.make(
        createLocButton,
        "Configuration error.  There is no configuration entry for LOC to support this machines with collimator " + collimator.model + " Contact the system administrator."
      )
    } else {
      val rtplanConfigBaselineErr = validateConfigAndRtplanFileExists(Some(locRtplan.get.baseline), machine, "LOC Baseline", createLocButton)
      val rtplanConfigDeliveryErr = validateConfigAndRtplanFileExists(Some(locRtplan.get.delivery), machine, "LOC Delivery", createLocButton)
      rtplanConfigBaselineErr ++ rtplanConfigDeliveryErr
    }

    validateEntryFields(valueMap) ++ rtplanConfigErr
  }

  private def showDownload(alListWithNames: Seq[(AttributeList, String)], procedureName: String, valueMap: ValueMapT, machine: Machine, response: Response): Unit = {

    /** Make a name look like a DICOM file name. */
    //noinspection RegExpSimplifiable
    def nameToDcm(name: String) = FileUtil.replaceInvalidFileNameCharacters(name.replace(' ', '-').trim, '_').replaceAll("__*", "_") + ".dcm"

    /** Change a name into a HTML valid id attribute. */
    //noinspection RegExpSimplifiable
    def nameToId(name: String) = "ID_" + name.replaceAll("[^a-zA-Z0-9]", "_")

    val downloadUrl: String = {
      val realMachineName = AnonymizeUtil.decryptWithNonce(machine.institutionPK, machine.id_real.get)

      def makeFile(suffix: String) = {
        val fileName = {
          val n = "RTPLAN_" + Util.timeAsFileName(new Date) + "_" + procedureName + "_" + realMachineName + suffix
          FileUtil.replaceInvalidFileNameCharacters(n.replace(' ', '_').trim, '_')
        }
        new File(Config.tmpDirFile, fileName)
      }

      if (alListWithNames.size == 1) {
        val file = makeFile(".dcm")
        DicomUtil.writeAttributeListToFile(alListWithNames.head._1, file, "AQA")
        WebServer.urlOfTmpFile(file)
      } else {
        val file = makeFile(".zip")
        val alNameList = alListWithNames.map(alName => (alName._1, nameToDcm(alName._2)))
        val zippedContent = DicomUtil.namedDicomToZippedByteArray(alNameList, "AQA")
        FileUtil.writeBinaryFile(file, zippedContent)
        WebServer.urlOfTmpFile(file)
      }
    }

    val downloadLink = new WebPlainText("Download", false, 3, 0, _ => { <h4> <a href={downloadUrl} title="Click to download DICOM RTPLAN file(s).">Download</a></h4> })

    val dicomNav = {

      def nameToNav(name: String) = {
        <a href={"#" + nameToId(name)} style="margin-right: 25px;">Go to: {name}</a>
      }

      if (alListWithNames.size > 1) {
        <span>
          {alListWithNames.map(alName => nameToNav(alName._2))}
        </span>
      } else {
        <span>
        </span>
      }
    }

    val dicomViewHtml = {
      def toHtml(al: AttributeList, name: String) = {
        <div>
          {dicomNav}
          <span id={nameToId(name)}><h4><p/>Preview {name}</h4><p/><pre title="DICOM meta-data">{WebUtil.nl + DicomUtil.attributeListToString(al)}</pre></span>
        </div>
      }

      val elem = {
        <div>
          {alListWithNames.map(rtplanName => toHtml(rtplanName._1, rtplanName._2))}
        </div>
      }
      elem
    }

    val dicomView = new WebPlainText("Download", false, 10, 0, _ => dicomViewHtml)

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
      CustomizeRtPlan.PlanSpecification(valueMap(toleranceTable.label), valueMap(patientID.label), valueMap(patientName.label), valueMap(machineName.label), valueMap(planName.label))
    planSpecification
  }

  /**
    * Show the user a message saying why the creation of a custom rtplan failed.
    */
  private def showFailedCustomize(valueMap: ValueMapT, styleMap: StyleMapT, response: Response): Unit = {
    val form = new WebForm(pathOf, List(row0, row1, row2) ++ List(assignButtonList))
    form.setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  private def validateAndMakePhase2Plan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = validatePhase2(valueMap)
    if (styleMap.nonEmpty) {
      showFailedCustomize(valueMap, styleMap, response)
    } else {
      val userPK = getUser(valueMap).get.userPK.get
      val planSpecification = makePlanSpec(valueMap)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
      val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
      val rtplan = CustomizeRtPlan.makePlanPhase2(machine, userPK, planSpecification, machineEnergyList)
      showDownload(Seq((rtplan, "Phase 2")), "Phase2", valueMap, machine, response)
    }
  }

  private def validateAndMakePhase3Plan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = validatePhase3(valueMap)
    if (styleMap.nonEmpty) {
      showFailedCustomize(valueMap, styleMap, response)
    } else {
      val userPK = getUser(valueMap).get.userPK.get
      val planSpecification = makePlanSpec(valueMap)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
      val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
      val rtplan = CustomizeRtPlan.makePlanPhase3(machine, userPK, planSpecification, machineEnergyList)
      showDownload(Seq((rtplan, "Phase 2")), "Phase2", valueMap, machine, response)
    }
  }

  private def validateAndMakeLocPlan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = validateLOC(valueMap)
    if (styleMap.nonEmpty) {
      showFailedCustomize(valueMap, styleMap, response)
    } else {
      val userPK = getUser(valueMap).get.userPK.get
      val planSpecification = makePlanSpec(valueMap)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
      val locPair = CustomizeRtPlan.makePlanLOC(machine, userPK, planSpecification)
      showDownload(Seq((locPair.baselineAl, "LOC Baseline"), (locPair.deliveryAl, "LOC Delivery")), "LOC", valueMap, machine, response)
    }
  }

  private def validateAndMakeDailyQAPlan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = validateDailyQA(valueMap)
    if (styleMap.nonEmpty) {
      showFailedCustomize(valueMap, styleMap, response)
    } else {
      val userPK = getUser(valueMap).get.userPK.get
      val planSpecification = makePlanSpec(valueMap)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
      val rtplan = CustomizeRtPlan.makePlanDailyQA(machine, userPK, planSpecification)
      showDownload(Seq((rtplan, "Daily QA")), "Daily QA", valueMap, machine, response)
    }
  }

  private def validateAndMakeGapSkewPlan(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = validateGapSkew(valueMap)
    if (styleMap.nonEmpty) {
      showFailedCustomize(valueMap, styleMap, response)
    } else {
      val userPK = getUser(valueMap).get.userPK.get
      val planSpecification = makePlanSpec(valueMap)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
      val rtplan = CustomizeRtPlan.makePlanGapSkew(machine, userPK, planSpecification)
      showDownload(Seq((rtplan, "Gap Skew")), "Gap Skew", valueMap, machine, response)
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
        case _ if buttonIs(valueMap, createPhase2Button)                                                         => validateAndMakePhase2Plan(valueMap, response)
        case _ if buttonIs(valueMap, createPhase3Button)                                                         => validateAndMakePhase3Plan(valueMap, response)
        case _ if buttonIs(valueMap, createLocButton)                                                            => validateAndMakeLocPlan(valueMap, response)
        case _ if buttonIs(valueMap, createDailyQAButton)                                                        => validateAndMakeDailyQAPlan(valueMap, response)
        case _ if buttonIs(valueMap, createGapSkewButton)                                                        => validateAndMakeGapSkewPlan(valueMap, response)
        case _                                                                                                   => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
