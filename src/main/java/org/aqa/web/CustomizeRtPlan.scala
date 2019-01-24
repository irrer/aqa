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
import org.aqa.db.MachineBeamEnergy
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import org.aqa.AnonymizeUtil
import com.pixelmed.dicom.ValueRepresentation
import com.pixelmed.dicom.AttributeTag
import edu.umro.util.UMROGUID
import java.io.File

object CustomizeRtPlan {
  def reference(machinePK: Long) = { (new CustomizeRtPlan).pathOf + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  def redirect(machinePK: Long, response: Response): Unit = response.redirectSeeOther(reference(machinePK))

  def redirect(valueMap: ValueMapT, response: Response): Unit = redirect(valueMap(MachineUpdate.machinePKTag).toLong, response)
}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlan extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

  private val machineIdTag = "MachineId"

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
    <h3 title="Plan customization for machine">Create Custom RT Plan Comptatible with Machine { linkToMachineUpdate(valueMap) }</h3>
  }

  val machineId = new WebPlainText(machineIdTag, false, 6, 0, machineIdHtml)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match name in planning system", false)

  private def machineName = new WebInputText("Machine Name", true, 2, 0, "To match planning system", false)

  private def patientID = new WebInputText("Patient ID", true, 2, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private val row0: WebRow = List(machineId)
  private val row2: WebRow = List(toleranceTable, machineName)
  private val row1: WebRow = List(patientID, patientName)

  private def getCollimatorCompatiblePlanForMachine(machine: Machine) = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFile = Config.Phase2PlanFileList.filter(pf => pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) && pf.model.equalsIgnoreCase(collimator.model)).head
    planFile
  }

  private def getPlanEnergyList(machine: Machine): Seq[Double] = {
    val planAttrList = getCollimatorCompatiblePlanForMachine(machine).dicomFile.attributeList.get
    val attrList = DicomUtil.findAllSingle(planAttrList, TagFromName.NominalBeamEnergy)
    val list = attrList.map(a => a.getDoubleValues.head).distinct.sorted
    logger.info("Energy list found in plan: " + list.mkString("  "))
    list
  }

  private def getMachineEnergyList(machinePK: Long): Seq[Double] = {
    val list = MachineBeamEnergy.getByMachine(machinePK).map(mbe => mbe.photonEnergy_MeV).flatten.sorted
    list
  }

  private def getMachineEnergySelections(machineEnergyList: Seq[Double]): Seq[(String, String)] = {
    val pairs = machineEnergyList.zipWithIndex.map(ei => (ei._2.toString, Util.fmtDbl(ei._1)))
    pairs
  }

  private val machEnergyPrefix = "MachEnergy"
  private def machEnergyTag(index: Int) = machEnergyPrefix + index

  private def energyRowList(machine: Machine, machineEnergyList: Seq[Double], planEnergyList: Seq[Double]): List[WebRow] = {
    val machineEnergySelectionList = getMachineEnergySelections(machineEnergyList)
    val planEnergyList = getPlanEnergyList(machine).toList
    val rowList = planEnergyList.indices.toList.map(i => {
      val label = "For plan energy " + Util.fmtDbl(planEnergyList(i)) + " use machine energy: "
      val energyPlan = new WebPlainText(label, false, 3, 0, (ValueMapT) => { <span title="Choose the machine energy to use for the planned energy">{ label }</span> })
      val energyMach = new WebInputSelect(machEnergyTag(i), false, 1, 0, (_) => machineEnergySelectionList, false)
      val row: WebRow = List(energyPlan, energyMach)
      row
    })
    rowList
  }

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", false, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)
  private val backButton = makeButton("Back", false, ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(createButton, cancelButton, machinePK)

  /**
   * Find the machine energy value closest to each plan energy value and assign it.
   */
  private def automaticEnergyAssignments(valueMap: ValueMapT, machineEnergyList: Seq[Double], planEnergyList: Seq[Double]) = {
    def closestMachine(planEnergy: Double) = {
      val closest = machineEnergyList.minBy(machEnergy => (machEnergy - planEnergy).abs)
      machineEnergyList.indexOf(closest)
    }
    val energyMap = planEnergyList.indices.map(p => (machEnergyTag(p), closestMachine(planEnergyList(p)).toString)).toMap
    energyMap
  }

  private def formSelect(valueMap: ValueMapT, response: Response) = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
    val planEnergyList = getPlanEnergyList(machine).toList
    val form = new WebForm(pathOf, List(row0, row1, row2) ++ energyRowList(machine, machineEnergyList, planEnergyList) ++ List(assignButtonList))

    // if field is empty
    def empty(label: String) = valueMap.get(label).isEmpty || (valueMap(label).trim.size == 0)

    val defaultPatient = "$AQA_" + machine.machinePK.get

    def getRealMachineId = AnonymizeUtil.decryptWithNonce(machine.institutionPK, machine.id_real.get)

    val assignValMap = if (empty(machEnergyTag(0))) automaticEnergyAssignments(valueMap, machineEnergyList, planEnergyList) else emptyValueMap
    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val machineNameMap = if (empty(machineName.label)) Map((machineName.label, getRealMachineId)) else emptyValueMap

    val valMap = valueMap ++ assignValMap ++ patientIdMap ++ patientNameMap ++ machineNameMap
    form.setFormResponse(valMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  /**
   * Make sure fields are valid.
   */
  private def validate(valueMap: ValueMapT): StyleMapT = {
    // if field is empty
    def empty(label: String) = valueMap.get(label).isEmpty || (valueMap(label).trim.size == 0)

    val tolErr = if (empty(toleranceTable.label)) Error.make(toleranceTable, "A tolerance table name must be given.") else styleNone
    val machErr = if (empty(machineName.label)) Error.make(machineName, "A machine name must be given.") else styleNone
    val patIdErr = if (empty(patientID.label)) Error.make(patientID, "A patient ID must be given.") else styleNone
    val patNameErr = if (empty(patientName.label)) Error.make(patientName, "A patient name must be given.") else styleNone
    val machineNameErr = if (empty(machineName.label)) Error.make(machineName, "A machine name must be given.") else styleNone

    (tolErr ++ patIdErr ++ patNameErr)
  }

  private def replaceAllUids(attributeList: AttributeList) = {
    // get list of attributes that are UIDs
    val uidSet = Config.ToBeAnonymizedList.keySet.filter(tag => ValueRepresentation.isUniqueIdentifierVR(DicomUtil.dictionary.getValueRepresentationFromTag(tag)))
    val attrList = DicomUtil.findAll(attributeList, uidSet)
    val replaceMap = attrList.map(at => at.getSingleStringValueOrEmptyString).distinct.map(uid => (uid, UMROGUID.getUID)).toMap
    def replace(at: Attribute) = {
      val uid = replaceMap(at.getSingleStringValueOrEmptyString)
      at.removeValues
      at.addValue(uid)
    }
    attrList.map(at => replace(at))
  }

  private def showDownload(rtplan: AttributeList, valueMap: ValueMapT, machine: Machine, response: Response) = {

    val sopuid = Util.sopOfAl(rtplan)
    val file = new File(Config.tmpDirFile, sopuid + ".dcm")
    DicomUtil.writeAttributeList(rtplan, file)
    val downloadUrl = WebServer.urlOfTmpFile(file)

    val downloadLink = new WebPlainText("Download", false, 3, 0, (ValueMapT) => { <h4> <a href={ downloadUrl } title="Click to download DICOM RTPLAN file.">Download</a></h4> })

    val dicomViewHtml = { <span><h4><p/>Preview</h4><p/><pre title="DICOM meta-data">{ WebUtil.nl + DicomUtil.attributeListToString(rtplan) }</pre></span> }
    val dicomView = new WebPlainText("Download", false, 10, 0, (ValueMapT) => dicomViewHtml)

    val r1: WebRow = List(downloadLink, backButton, machinePK)
    val r2: WebRow = List(dicomView)
    val form = new WebForm(pathOf, List(row0, r1, r2))

    form.setFormResponse(valueMap, styleNone, "Download RTPLAN", response, Status.SUCCESS_OK)
  }

  /**
   * Save an anonymized version of this file, so when the user uploads RTIMAGEs generated by
   * this plan, the plan will already be uploaded.
   */
  private def saveAnonymizedDicom(institutionPK: Long, rtplan: AttributeList) = {
    val anon = AnonymizeUtil.anonymizeDicom(institutionPK, rtplan)
    val file = new File(Config.sharedDir, Util.sopOfAl(anon) + ".dcm")
    DicomUtil.writeAttributeList(anon, file)
  }

  private def makePlan(valueMap: ValueMapT, response: Response, machine: Machine, planEnergyList: Seq[Double], machineEnergyList: Seq[Double]) = {

    val rtplan = DicomUtil.clone(getCollimatorCompatiblePlanForMachine(machine).dicomFile.attributeList.get)
    replaceAllUids(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    new Config.PlanAttributeOverride(TagFromName.ToleranceTableLabel, valueMap(toleranceTable.label))

    val overrideList = Config.Phase2PlanAttributeOverrideList ++ Seq(
      new Config.PlanAttributeOverride(TagFromName.ToleranceTableLabel, valueMap(toleranceTable.label)),
      new Config.PlanAttributeOverride(TagFromName.PatientID, valueMap(patientID.label).trim),
      new Config.PlanAttributeOverride(TagFromName.PatientName, valueMap(patientName.label).trim),
      new Config.PlanAttributeOverride(TagFromName.TreatmentMachineName, valueMap(machineName.label).trim))

    // modify all attributes that get a constant value
    overrideList.map(ov => {
      DicomUtil.findAllSingle(rtplan, ov.tag).map(at => { at.removeValues; at.addValue(ov.value) })
    })

    def indexToEnergyPair(key: String) = {
      val plan = planEnergyList(key.replace(machEnergyPrefix, "").toInt)
      val mach = machineEnergyList(valueMap(key).toInt)
      (plan, mach)
    }

    val energyMap = valueMap.keys.filter(key => key.startsWith(machEnergyPrefix)).map(key => indexToEnergyPair(key)).toMap

    def changeEnergy(at: Attribute) = {
      val energy = energyMap(at.getDoubleValues.head)
      at.removeValues
      at.addValue(energy)
    }
    DicomUtil.findAllSingle(rtplan, TagFromName.NominalBeamEnergy).map(at => changeEnergy(at))

    saveAnonymizedDicom(machine.institutionPK, rtplan)

    showDownload(rtplan, valueMap, machine, response)
  }

  private def createPlan(valueMap: ValueMapT, response: Response) = {
    val styleMap = validate(valueMap)
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
    val planEnergyList = getPlanEnergyList(machine).toList
    if (styleMap.nonEmpty) {
      val form = new WebForm(pathOf, List(row0, row1, row2) ++ energyRowList(machine, machineEnergyList, planEnergyList) ++ List(assignButtonList))
      form.setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
    } else {
      makePlan(valueMap, response, machine, planEnergyList, machineEnergyList)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {
      val user = CachedUser.get(request)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach = MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        case _ if user.isEmpty => updateMach
        case _ if machine.isEmpty => updateMach
        case _ if (user.get.institutionPK != machine.get.institutionPK) => updateMach
        case _ if buttonIs(valueMap, cancelButton) => updateMach
        case _ if buttonIs(valueMap, backButton) => formSelect(valueMap, response)
        case _ if buttonIs(valueMap, createButton) => createPlan(valueMap, response)
        case _ => formSelect(valueMap, response) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }
}
