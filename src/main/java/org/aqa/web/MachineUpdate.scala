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

package org.aqa.web

import org.aqa.AnonymizeUtil
import org.aqa.db.CachedUser
import org.aqa.db.EPID
import org.aqa.db.Input
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.db.MachineType
import org.aqa.db.MaintenanceRecord
import org.aqa.db.MultileafCollimator
import org.aqa.web.WebUtil._
import org.aqa.webrun.phase2.customizeRtPlan.CustomizeRtPlanInterface
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.Elem

object MachineUpdate {
  val machinePKTag = "machinePK"

  def machineReference(machinePK: Long): String = { "/" + SubUrl.admin + "/" + "MachineUpdate" + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  def linkToMachineUpdate(machinePK: Long, machineId: String): Elem = {
    <a href={machineReference(machinePK)} aqaalias="">{machineId}</a>
  }

  def redirect(machinePK: Long, response: Response): Unit = {
    response.redirectSeeOther(machineReference(machinePK))
  }
}

class MachineUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleEdit = "Edit Machine"

  private val id = new WebInputText("Id", true, 3, 0, "Name of machine (required)", true)

  private val tpsId = new WebInputText("TPS ID", true, 3, 0, "ID of machine that is unique within the treatment planning system.", true)

  private val machineTypePK = new WebInputSelect("Type", true, 3, 0, typeName, false)

  //noinspection ScalaUnusedSymbol
  private def typeName(response: Option[Response]) = MachineType.list.toList.map(mt => (mt.machineTypePK.get.toString, mt.toName))

  //noinspection ScalaUnusedSymbol
  private def collimatorName(response: Option[Response]) = MultileafCollimator.list.toList.map(mlc => (mlc.multileafCollimatorPK.get.toString, mlc.toName))

  //noinspection ScalaUnusedSymbol
  private def epidName(response: Option[Response]) = EPID.list.toList.map(e => (e.epidPK.get.toString, e.toName))

  private def getConfigUrl(valueMap: ValueMapT): Elem = {
    val notDef = { <div>Configuration directory not defined</div> }
    val machPk = valueMap.get(machinePK.label)
    if (machPk.isDefined && machPk.get.nonEmpty) {
      val mach = Machine.get(machPk.get.toLong)
      if (mach.isDefined) {
        val machConfigDir = mach.get.configurationDirectory
        if (machConfigDir.isDefined) {
          val configDirName = mach.get.configurationDirectory.get
          <a href={WebServer.urlOfMachineConfigurationPath(configDirName)}>Configuration Directory</a>
        } else notDef
      } else notDef
    } else notDef
  }

  private def getSerialNo(valueMap: ValueMapT): Elem = {
    val notDefTitle =
      "The DICOM device serial number (0018,1000) has not been " + titleNewline +
        "determined for this machine because no tests have been " + titleNewline +
        "run against it.  It is established when a test is run and " + titleNewline +
        "this machine is chosen to be associated with the uploaded DICOM" + titleNewline +
        "file(s). This option is only available to administrators."
    val defTitle =
      "The DICOM device serial number (0018,1000) has been " + titleNewline +
        "determined for this machine because it was chosen from" + titleNewline +
        "a list of machines when running a test.  This option is only available" + titleNewline +
        "to administrators."
    val notDef = <div title={notDefTitle}>{nbsp + " " + nbsp + " " + nbsp + " " + nbsp + " "}Serial number <em>not defined</em></div>
    val machPk = valueMap.get(machinePK.label)
    if (machPk.isDefined) {
      val mach = Machine.get(machPk.get.toLong)
      if (mach.isDefined && mach.get.serialNumber.isDefined) {
        <div title={defTitle}>Serial Number {mach.get.serialNumber.get}</div>
      } else notDef
    } else notDef
  }

  private val configurationDirectory = new WebPlainText("Configuration", false, 6, 0, getConfigUrl)

  private val multileafCollimatorPK = new WebInputSelect("Collimator", true, 3, 0, collimatorName, false)

  private val epidPK = new WebInputSelect("EPID", true, 3, 0, epidName, false)

  private val serialNumber = new WebPlainText("Serial Number", false, 2, 0, getSerialNo)

  private val serialNumberReset = {
    val title = "If this is checked, then when you click Save," + titleNewline +
      "the device serial number that is stored in the database" + titleNewline +
      "for this machine will be reset.  It will be set again the" + titleNewline +
      "next time a test is run and this machine is selected."
    new WebInputCheckbox("Reset Serial Number", true, Some(title), 3, 0)
  }

  private val onboardImager = new WebInputCheckbox("Onboard Imager", true, Some("Check to indicate that the machine " + titleNewline + "has an onboard imager"), 3, 0)
  private val table6DOF = new WebInputCheckbox("6DOF Table", true, Some("Check to indicate that the machine" + titleNewline + "has a 6 degrees of freedom table"), 3, 0)
  private val developerMode = new WebInputCheckbox("Developer Mode", true, Some("Check to indicate that the machine" + titleNewline + " supports developer mode"), 3, 0)
  private val active = new WebInputCheckbox("Active", true, Some("Check to indicate that the machine" + titleNewline + " is in active use"), 3, 0)
  private val respiratoryManagement = new WebInputCheckbox("Respiratory Management", true, Some("Check to indicate that the machine supports" + titleNewline + " respiratory management"), 3, 0)
  private val imagingBeam2_5_mv = new WebInputCheckbox("Has 2.5 mv imaging beam", true, Some("Check to indicate that the machine " + titleNewline + "has a 2.5 mv imaging beam"), 3, 0)

  private def showConfirmDelete(valueMap: ValueMapT): Elem = {
    val machPK = valueMap(machinePK.label).toLong
    val mach = Machine.get(machPK).get
    val inputList = Input.getByMachine(machPK)
    val maintenanceList = MaintenanceRecord.getByMachine(machPK)
    val html = {
      <div>If machine <span aqaalias="">{mach.id}</span> is deleted then {inputList.size} data sets and {maintenanceList.size} maintenance records will also be deleted. </div>
    }
    html
  }

  private val confirmDeleteMessage = new WebPlainText("Confirm Delete", true, 6, 0, showConfirmDelete)

  /**
    * Get the list of institutions that the user may choose from.
    */
  private def institutionList(response: Option[Response]): Seq[(String, String)] = {
    val request = if (response.isDefined) Some(response.get.getRequest) else None
    val isWhitelisted = request.isDefined && userIsWhitelisted(request.get)

    def instToChoice(inst: Institution) = (inst.institutionPK.get.toString, inst.name)
    if (isWhitelisted) Institution.list.sortBy(_.name).map(instToChoice)
    else {
      if (request.isDefined) {
        val valueMap = getValueMap(request.get)
        val machPK = valueMap.get(machinePK.label)
        if (machPK.isDefined) {
          // this is an update of an existing machine
          val instPK = Machine.get(machPK.get.toLong).get.institutionPK
          Seq((instPK.toString, Institution.get(instPK).get.name))
        } else {
          // this is the creation of a new machine.  The only choice they have is their own institution.
          val instPK = getUser(request.get).get.institutionPK
          Seq((instPK.toString, Institution.get(instPK).get.name))
        }
      } else {
        // Could not determine what the user was authorized to see or what the machine was or the institution, so give them no choices
        Seq[(String, String)]()
      }
    }
  }

  private val institutionPK = new WebInputSelect("Institution", true, col = 3, 0, institutionList, true)

  private val notes = new WebInputTextArea("Notes", 6, 0, "", true)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val columns = (name.length / 15) + 1
    new FormButton(name, columns, 0, subUrl, pathOf, buttonType)
  }

  private val saveButton = makeButton("Save", primary = true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", primary = false, ButtonType.BtnDanger)
  private val confirmDeleteButton = makeButton("Confirm Delete", primary = false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)
  private val maintenanceButton = makeButton("Maintenance Records", primary = false, ButtonType.BtnDefault)
  private val dailyQAButton = makeButton("Daily QA", primary = false, ButtonType.BtnDefault)
  private val customizePlanButton = makeButton("Customize Plan", primary = false, ButtonType.BtnDefault)
  private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  //   class WebPlainText(override val label: String, val showLabel: Boolean, col: Int, offset: Int, html: (Any) => Elem) extends IsInput(label) with ToHtml {

  private val photonEnergyHeader = new WebPlainText("Photon Energy", true, 1, 1, _ => { <div title="In MeV"></div> })
  private val maxDoseRateHeader = new WebPlainText("Max Dose Rate", true, 1, 0, _ => { <div title="In MU / minute"></div> })
  private val fffEnergyHeader =
    new WebPlainText("FFF energy", true, 1, 0, _ => { <div title={"Check to indicate that this is an" + WebUtil.titleNewline + " FFF (Flattening Free Filter) beam"}></div> })
  val addBeamEnergyButton = new FormButton("Add Beam Energy", 1, 0, subUrl, pathOf + "?addBeam=1", ButtonType.BtnPrimary)

  val photonEnergyColName = "Photon Energy_"
  val maxDoseRateColName = "Max Dose Rate_"
  val fffEnergyColName = "FFF Energy_"
  val beamEnergyButtonColName = "X_"

  def makeBeamValueSet(index: Int): ValueMapT = {
    Map(
      (photonEnergyColName + index.toString, ""),
      (maxDoseRateColName + index.toString, ""),
      (fffEnergyColName + index.toString, ""),
      (beamEnergyButtonColName + index.toString, index.toString)
    )
  }

  def makeBeamRow(index: Int): WebRow = {
    val photonEnergyCol = new WebInputText("Photon Energy_" + index, false, 1, 1, "In Mev")
    val maxDoseRateCol = new WebInputText("Max Dose Rate_" + index, false, 1, 0, "In MU / minute")
    val fffEnergyCol = new WebInputCheckbox(fffEnergyColName + index, false, 1, 0)
    val action = pathOf + "?beamIndex=" + index
    val beamDeleteButton = new FormButton("X", 1, 0, subUrl, action, ButtonType.BtnDefault, beamEnergyButtonColName + index)
    val indexCol = new WebInputHidden(index.toString)

    List(photonEnergyCol, maxDoseRateCol, fffEnergyCol, beamDeleteButton, indexCol)
  }

  def getBeamList(valueMap: ValueMapT): List[Int] = {
    valueMap.keySet.filter { k => k.startsWith(photonEnergyColName) }.map(k => k.replaceAll(".*_", "").toInt).toList.sorted
  }

  private def beamEnergyRows(valueMap: ValueMapT): List[WebRow] = {
    val minSize = 4 // minimum number of beam energies to display
    val beamList = {
      val bl = getBeamList(valueMap)
      if (bl.size < minSize) {
        val max = if (bl.isEmpty) 0 else bl.max
        bl ++ (1 to (minSize - bl.size)).map(b => b + max).toList
      } else bl
    }
    beamList.map(b => makeBeamRow(b))
  }

  def fieldList(valueMap: ValueMapT, isAdmin: Boolean): List[WebRow] = {
    val listSerNo: List[WebRow] = {
      if (isAdmin) List(List(serialNumber, serialNumberReset))
      else List[WebRow]()
    }
    val listA: List[WebRow] = List(
      List(id, machineTypePK),
      List(multileafCollimatorPK, epidPK),
      List(configurationDirectory),
      List(institutionPK, tpsId),
      List(onboardImager, table6DOF),
      List(developerMode, respiratoryManagement),
      List(imagingBeam2_5_mv, active),
      List(photonEnergyHeader, maxDoseRateHeader, fffEnergyHeader, addBeamEnergyButton)
    )

    val listB: List[WebRow] = beamEnergyRows(valueMap)
    val listC: List[WebRow] = List(List(notes))

    listA ++ listB ++ listC ++ listSerNo
  }

  val editButtonList1: WebRow = List(saveButton, cancelButton, deleteButton)
  val editButtonList2: WebRow = List(maintenanceButton, dailyQAButton, customizePlanButton, machinePK)
  val confirmDeleteButtonList: WebRow = List(cancelButton, confirmDeleteButton, machinePK)

  def confirmDeleteFieldList(): List[WebRow] = {
    val list: List[WebRow] = List(List(confirmDeleteMessage))
    list
  }

  private def formEdit(valueMap: ValueMapT, isAdmin: Boolean) = new WebForm(pathOf, fieldList(valueMap, isAdmin) ++ Seq(editButtonList1, editButtonList2))

  private def formConfirmDelete() = new WebForm(pathOf, confirmDeleteFieldList() :+ confirmDeleteButtonList)

  private def emptyId(valueMap: ValueMapT): StyleMapT = {
    val idText = valueMap(id.label).trim
    if (idText.trim.isEmpty) {
      Error.make(id, "Id can not be empty")
    } else styleNone
  }

  /**
    * Check that id is unique within institution
    */
  private def validateUniqueness(valueMap: ValueMapT): StyleMapT = {
    val instPK = valueMap(institutionPK.label).toLong
    val machID = valueMap(id.label).trim
    val machPK = valueMap.get(machinePK.label)

    val machList = {
      val sameIDList = Machine.listMachinesFromInstitution(instPK).filter(m => AnonymizeUtil.decryptWithNonce(instPK, m.id_real.get).equalsIgnoreCase(machID))
      if (machPK.isDefined && machPK.get.nonEmpty)
        sameIDList.filter(m => m.machinePK.get != machPK.get.toInt)
      else
        sameIDList
    }

    if (machList.isEmpty) styleNone
    else Error.make(id, "There is already a machine with named " + machID + " at this institution")
  }

  /**
    * Check that the beam energies specified by the user are valid.  All photon energies must be specified, but
    * max dose and FFF may be blank.  All must be floating point values greater than 0.  At least one energy must
    * be specified.
    */
  private def validateBeamEnergies(valueMap: ValueMapT, form: WebForm): StyleMapT = {

    val beamList = getBeamList(valueMap)

    def checkEnergy(label: String, required: Boolean): StyleMapT = {
      val input = form.findInput(label).get
      val text = valueMap(label).trim
      stringToDouble(text) match {
        case None if text.nonEmpty => Error.make(input, "Invalid floating point number")
        case None if required      => Error.make(input, "Energy is required")
        case None if !required     => styleNone
        case Some(d) if 0 >= d     => Error.make(input, "Energy must be greater than 0")
        case Some(_)               => styleNone;
      }
    }

    def isEmpty(index: Int): Boolean = {
      val phoLabel = photonEnergyColName + index
      val maxLabel = maxDoseRateColName + index

      (valueMap(phoLabel).trim + valueMap(maxLabel).trim).isEmpty
    }

    def checkBeam(index: Int): StyleMapT = {
      val phoLabel = photonEnergyColName + index
      val maxLabel = maxDoseRateColName + index

      // if all values are empty, then ignore it
      if ((valueMap(phoLabel).trim + valueMap(phoLabel).trim + valueMap(phoLabel).trim).isEmpty) styleNone
      else checkEnergy(phoLabel, required = true) ++ checkEnergy(maxLabel, required = false)
    }

    val allEmpty = beamList.map(i => isEmpty(i)).reduce(_ && _)
    if (allEmpty) Error.make(addBeamEnergyButton, "At least one energy level is required")
    else beamList.map(i => checkBeam(i)).reduce(_ ++ _)
  }

  private def validateAuthentication(valueMap: ValueMapT, request: Request): StyleMapT = {
    val instPK = valueMap(institutionPK.label).trim.toLong
    if (isAuthorized(request, instPK)) styleNone
    else Error.make(institutionPK, "You are not allowed to modify machines from other institutions.")
  }

  private def validateAll(valueMap: ValueMapT, request: Request, form: WebForm): StyleMapT = {
    val auth = validateAuthentication(valueMap, request)
    if (auth.isEmpty) {
      emptyId(valueMap) ++
        validateUniqueness(valueMap) ++
        validateBeamEnergies(valueMap, form)
    } else { emptyId(valueMap) ++ auth }
  }

  private def updateMachineInDatabase(valueMap: ValueMapT): Unit = {
    val machine = constructMachineFromParameters(valueMap)
    val mach: Machine = if (machine.machinePK.isDefined) {
      machine.insertOrUpdate()
      machine
    } else {
      val machWithPk = machine.insert
      val idVal = AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasPrefixId, machWithPk.machinePK.get)
      val machWithId = machWithPk.copy(id = idVal)
      machWithId.insertOrUpdate()
      machWithId
    }
    logger.info("Updating machine " + mach)
    updateBeamEnergies(mach, valueMap)
  }

  private def userIsAdmin(response: Response): Boolean = {
    getUserIdOrDefault(response.getRequest, "").toLowerCase.contains("admin")
  }

  /**
    * Save changes made to form.
    */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    val form = formEdit(valueMap, userIsAdmin(response))
    val styleMap = validateAll(valueMap, response.getRequest, form)
    if (styleMap.isEmpty) {
      updateMachineInDatabase(valueMap)
      MachineList.redirect(response)
    } else {
      form.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  private def updateBeamEnergies(machine: Machine, valueMap: ValueMapT): Unit = {

    def makeBeam(index: Int): Option[MachineBeamEnergy] = {
      val pho = valueMap(photonEnergyColName + index)
      val max = valueMap(maxDoseRateColName + index)
      val fffVal = if (valueMap.contains(fffEnergyColName + index)) 1.0 else 0.0

      val valList = Seq(pho, max)
      val viable = valList.exists { x => x.trim.nonEmpty } || (fffVal != 0.0)

      if (viable) {
        Some(new MachineBeamEnergy(None, machine.machinePK.get, stringToDouble(pho), stringToDouble(max), Some(fffVal)))
      } else
        None
    }

    // The way that the user wants the beam energies
    val requestedBeamList = getBeamList(valueMap).flatMap(b => makeBeam(b))

    // The beam energies that are in the database now
    val existInDatabase = MachineBeamEnergy.getByMachine(machine.machinePK.get)

    requestedBeamList.diff(existInDatabase).map(be => be.insert)
    existInDatabase.diff(requestedBeamList).map(be => MachineBeamEnergy.delete(be.machineBeamEnergyPK.get))
  }

  /**
    * If the user is authorized to modify the machine in this way then return true.
    *
    * The two allowed cases are:
    *
    *     - user is whitelisted
    *
    *     - user is modifying a machine that is in their institution
    */
  private def isAuthorized(request: Request, institutionPK: Long): Boolean = {
    val ok = WebUtil.userIsWhitelisted(request) || {
      getUser(request) match {
        case Some(user) => (user.institutionPK == institutionPK) || WebUtil.userIsWhitelisted(request)
        case _          => false
      }
    }
    ok
  }

  /**
    * Create a new machine
    */
  private def constructMachineFromParameters(valueMap: ValueMapT): Machine = {
    val institutionPKVal = valueMap(institutionPK.label).trim.toLong
    val pk: Option[Long] = {
      val e = valueMap.get(machinePK.label)
      if (e.isDefined && e.get.trim.nonEmpty) Some(e.get.toLong) else None
    }
    val idVal = if (pk.isDefined) AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasPrefixId, pk.get) else "None"
    val id_realVal = Some(AnonymizeUtil.encryptWithNonce(institutionPKVal, valueMap(id.label).trim))

    val tpsIdVal: Option[String] = {
      valueMap(tpsId.label).trim match {
        case text if text.isEmpty => None
        case text                 => Some(AnonymizeUtil.encryptWithNonce(institutionPKVal, text))
      }

    }

    val machineTypePKVal = valueMap(machineTypePK.label).trim.toLong
    val multiLeafCollimatorPKVal = valueMap(multileafCollimatorPK.label).trim.toLong
    val epidPKVal = valueMap(epidPK.label).trim.toLong

    val serialNumberVal = {
      if (pk.isDefined && !valueMap.contains(serialNumberReset.label)) Machine.get(pk.get).get.serialNumber
      else None
    }

    val notesVal = AnonymizeUtil.encryptWithNonce(institutionPKVal, valueMap(notes.label).trim)

    val configDir: Option[String] = {
      if (pk.isDefined) Machine.get(pk.get).get.configurationDirectory
      else None
    }

    val machine = new Machine(
      pk,
      idVal,
      id_realVal,
      machineTypePKVal,
      configDir,
      multiLeafCollimatorPKVal,
      epidPKVal,
      institutionPKVal,
      serialNumberVal,
      valueMap.contains(imagingBeam2_5_mv.label),
      valueMap.contains(onboardImager.label),
      valueMap.contains(table6DOF.label),
      valueMap.contains(respiratoryManagement.label),
      valueMap.contains(developerMode.label),
      valueMap.contains(active.label),
      tpsIdVal,
      notesVal
    )

    machine
  }

  /**
    * Show this when machine asks to create a new machine from machine list.
    */
  private def emptyForm(response: Response): Unit = {
    val valueMap = Map((active.label, "true"))
    formEdit(emptyValueMap, userIsAdmin(response)).setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  /**
    * Get the current beam energies for the given machine and convert them into a ValueMapT.  Sort them in order of photon energy.
    */
  private def getBeamEnergyListAsValueMap(machinePK: Long): ValueMapT = {

    val beList = MachineBeamEnergy.getByMachine(machinePK).sortWith(MachineBeamEnergy.sorter)

    def sf(d: Option[Double]): String = {
      d match {
        case Some(dbl) => dbl.toString
        case _         => ""
      }
    }

    def beToValueMap(be: MachineBeamEnergy, index: Int): ValueMapT = {
      val fff = be.fffEnergy_MeV.isDefined && (be.fffEnergy_MeV.get != 0)

      val map = Map(
        (photonEnergyColName + index, sf(be.photonEnergy_MeV)),
        (maxDoseRateColName + index, sf(be.maxDoseRate_MUperMin)),
        (beamEnergyButtonColName + index, index.toString)
      )

      if (fff) map ++ Map((fffEnergyColName + index, "true")) else map
    }

    beList.zipWithIndex.flatMap(beIndex => beToValueMap(beIndex._1, beIndex._2)).toMap
  }

  /**
    * Show the edit page by populating the fields with the given machine.
    */
  private def edit(editValueMap: ValueMapT, response: Response): Unit = {
    val pk = editValueMap(machinePK.label).toLong

    val mach = Machine.get(pk).get

    val tps = {
      if (mach.tpsID_real.isDefined)
        AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasTreatmentPlanningSystemId, mach.machinePK.get)
      else
        ""
    }

    val valueMap = Map(
      (id.label, mach.id),
      (machineTypePK.label, mach.machineTypePK.toString),
      (institutionPK.label, mach.institutionPK.toString),
      (multileafCollimatorPK.label, mach.multileafCollimatorPK.toString),
      (epidPK.label, mach.epidPK.toString),
      (serialNumber.label, mach.serialNumber.toString),
      (imagingBeam2_5_mv.label, mach.imagingBeam2_5_mv.toString),
      (onboardImager.label, mach.onboardImager.toString),
      (table6DOF.label, mach.table6DOF.toString),
      (respiratoryManagement.label, mach.respiratoryManagement.toString),
      (developerMode.label, mach.developerMode.toString),
      (active.label, mach.active.toString),
      (notes.label, AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasNotesPrefixId, pk)),
      (tpsId.label, tps),
      (machinePK.label, pk.toString)
    ) ++ getBeamEnergyListAsValueMap(mach.machinePK.get)

    formEdit(valueMap, userIsAdmin(response)).setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def delete(valueMap: ValueMapT, response: Response): Unit = {
    val machPK = valueMap(machinePK.label).toLong
    val inputList = Input.getByMachine(machPK)
    val maintenanceList = MaintenanceRecord.getByMachine(machPK)

    val styleMap = validateAuthentication(valueMap, response.getRequest)

    if (styleMap.isEmpty) {
      if (inputList.isEmpty && maintenanceList.isEmpty) {
        Machine.delete(machPK)
        MachineList.redirect(response)
      } else formConfirmDelete().setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    } else {
      formEdit(valueMap, userIsAdmin(response)).setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.SUCCESS_OK)
    }
  }

  private def confirmDelete(valueMap: ValueMapT, response: Response): Unit = {
    val machPK = valueMap(machinePK.label).toLong
    val inputList = Input.getByMachine(machPK)
    val maintenanceList = MaintenanceRecord.getByMachine(machPK)
    maintenanceList.map(m => MaintenanceRecord.delete(m.maintenanceRecordPK.get))
    inputList.map(input => Input.delete(input.inputPK.get))
    Machine.delete(machPK)
    MachineList.redirect(response)
  }

  private def maintenanceRec(valueMap: ValueMapT, response: Response): Unit = {
    val path = MaintenanceRecordList.path + "?machinePK=" + valueMap(machinePK.label)
    response.redirectSeeOther(path)
  }

  private def dailyQA(valueMap: ValueMapT, response: Response): Unit = {
    save(valueMap, response) // in case user made changes
    MachineDailyQAUpdate.redirect(valueMap(machinePK.label).trim.toLong, response)
  }

  /**
    * If the user is authorized, go to the page for creating customized plans.
    */
  private def customizePlan(valueMap: ValueMapT, response: Response): Unit = {
    val machinePK = valueMap(MachineUpdate.machinePKTag).toLong
    val machine = Machine.get(machinePK).get
    val user = CachedUser.get(response.getRequest).get
    if ((user.institutionPK == machine.institutionPK) || WebUtil.userIsWhitelisted(response.getRequest)) {
      save(valueMap, response) // in case user made changes
      val form = formEdit(valueMap, userIsAdmin(response))
      val styleMap = validateAll(valueMap, response.getRequest, form)
      if (styleMap.nonEmpty) {
        form.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
      } else {
        updateMachineInDatabase(valueMap) // in case user has made changes
        CustomizeRtPlanInterface.redirect(machinePK, response)
      }
    } else {
      val styleMap = Error.make(institutionPK, "Only people from the machine's institution are allowed to create customized plans")
      formEdit(valueMap, userIsAdmin(response)).setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.SUCCESS_OK)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  private def reload(valueMap: ValueMapT, response: Response): Unit = {
    formEdit(valueMap, userIsAdmin(response)).setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def addBeam(valueMap: ValueMapT, response: Response): Unit = {
    val beamList = getBeamList(valueMap)
    val beamValueMap =
      if (beamList.isEmpty) makeBeamValueSet(0)
      else makeBeamValueSet(beamList.max + 1)

    reload(valueMap ++ beamValueMap, response)
  }

  private def deleteBeam(valueMap: ValueMapT, response: Response): Unit = {
    val index = valueMap.values.filter(k => k.startsWith(beamEnergyButtonColName)).head.replace(beamEnergyButtonColName, "").toInt
    val nameList = Seq(photonEnergyColName, maxDoseRateColName, fffEnergyColName, beamEnergyButtonColName).map(n => n + index.toString)

    val vm = valueMap.filter(kv => !nameList.contains(kv._1))
    reload(vm, response)
  }

  def buttonIsDeleteBeamEnergyButton(valueMap: ValueMapT): Boolean = {
    valueMap.values.exists(k => k.startsWith(beamEnergyButtonColName))
  }

  /**
    * Determine if the incoming request is to edit.
    */
  private def isEdit(valueMap: ValueMapT): Boolean = valueMap.contains(machinePK.label)

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton)         => MachineList.redirect(response)
        case _ if buttonIs(valueMap, addBeamEnergyButton)  => addBeam(valueMap, response)
        case _ if buttonIsDeleteBeamEnergyButton(valueMap) => deleteBeam(valueMap, response)
        case _ if buttonIs(valueMap, saveButton)           => save(valueMap, response)
        case _ if buttonIs(valueMap, deleteButton)         => delete(valueMap, response)
        case _ if buttonIs(valueMap, confirmDeleteButton)  => confirmDelete(valueMap, response)
        case _ if buttonIs(valueMap, maintenanceButton)    => maintenanceRec(valueMap, response)
        case _ if buttonIs(valueMap, dailyQAButton)        => dailyQA(valueMap, response)
        case _ if buttonIs(valueMap, customizePlanButton)  => customizePlan(valueMap, response)
        case _ if isEdit(valueMap)                         => edit(valueMap, response)
        case _                                             => emptyForm(response)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
