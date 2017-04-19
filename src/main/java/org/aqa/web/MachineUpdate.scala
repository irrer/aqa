package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import org.aqa.db.Machine
import scala.concurrent.ExecutionContext.Implicits.global
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.aqa.Logging._
import org.aqa.db.Institution.InstitutionTable
import org.aqa.db.Institution
import org.aqa.db.MachineType
import edu.umro.ScalaUtil.Trace._
import org.aqa.db.MultileafCollimator
import org.aqa.db.EPID
import org.aqa.db.MachineBeamEnergy

object MachineUpdate {
    val machinePKTag = "machinePK"
}

class MachineUpdate extends Restlet with SubUrlAdmin {

    private val pageTitleCreate = "Create Machine"

    private val pageTitleEdit = "Edit Machine"

    private val id = new WebInputText("Id", 6, 0, "Name of machine (required)")

    private def typeName() = MachineType.list.toList.map(mt => (mt.machineTypePK.get.toString, mt.toName))

    private val machineTypePK = new WebInputSelect("Type", 6, 0, typeName)

    private def collimatorName() = MultileafCollimator.list.toList.map(mlc => (mlc.multileafCollimatorPK.get.toString, mlc.toName))

    private def epidName() = EPID.list.toList.map(e => (e.epidPK.get.toString, e.toName))

    private def getConfigUrl(valueMap: ValueMapT): Elem = {
        val notDef = { <div>Configuration directory not defined</div> }
        val machPk = valueMap.get(machinePK.label)
        if (machPk.isDefined) {
            val mach = Machine.get(machPk.get.toLong)
            if (mach.isDefined) {
                val machConfigDir = mach.get.configurationDirectory
                if (machConfigDir.isDefined) {
                    val configDirName = mach.get.configurationDirectory.get
                    <a href={ WebServer.urlOfMachineConfigurationPath(configDirName) }>Configuration Directory</a>
                }
                else notDef
            }
            else notDef
        }
        else notDef
    }

    private def getSerialNo(valueMap: ValueMapT): Elem = {
        val notDef = <div>Serial number <em>not defined</em></div>
        val machPk = valueMap.get(machinePK.label)
        if (machPk.isDefined) {
            val mach = Machine.get(machPk.get.toLong)
            if (mach.isDefined && mach.get.serialNumber.isDefined) {
                <div>Serial Number { mach.get.serialNumber.get }</div>
            }
            else notDef
        }
        else notDef
    }

    private val configurationDirectory = new WebPlainText("Configuration", false, 6, 0, getConfigUrl _)

    private val multileafCollimatorPK = new WebInputSelect("Collimator", 6, 0, collimatorName)

    private val epidPK = new WebInputSelect("EPID", 6, 0, epidName)

    private val serialNumber = new WebPlainText("Serial Number", false, 3, 0, getSerialNo _)

    private val imagingBeam2_5_mv = new WebInputCheckbox("Has 2.5 mv imaging beam", 3, 0)

    private val onboardImager = new WebInputCheckbox("Onboard Imager", 3, 0)
    private val sixDimTabletop = new WebInputCheckbox("Six Dim Tabletop", 3, 0)
    private val respiratoryManagement = new WebInputCheckbox("Respiratory Management", 3, 0)
    private val developerMode = new WebInputCheckbox("Developer Mode", 3, 0)

    private def institutionList() = Institution.list.toList.sortBy(_.name).map(i => (i.institutionPK.get.toString, i.name))

    private val institutionPK = new WebInputSelect("Institution", 6, 0, institutionList)

    private val notes = new WebInputTextArea("Notes", 6, 0, "")

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)
    private val maintenanceButton = makeButton("Maintenance Records", false, ButtonType.BtnDefault)
    private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

    private val machineBeamEnergyPKTag = "machineBeamEnergyPK"

    //   class WebPlainText(override val label: String, val showLabel: Boolean, col: Int, offset: Int, html: (Any) => Elem) extends IsInput(label) with ToHtml {

    private val photonEnergyHeader = new WebPlainText("Photon Energy", true, 1, 1, _ => { <div></div> })
    private val maxDoseRateHeader = new WebPlainText("Max Dose Rate", true, 1, 0, _ => { <div></div> })
    private val fffEnergyHeader = new WebPlainText("FFF energy", true, 1, 0, _ => { <div></div> })
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
            (beamEnergyButtonColName + index.toString, index.toString))
    }

    def makeBeamRow(index: Int): WebRow = {
        val photonEnergyCol = new WebInputText("Photon Energy_" + index, false, 1, 1, "In Mev")
        val maxDoseRateCol = new WebInputText("Max Dose Rate_" + index, false, 1, 0, "In MU / minute")
        val fffEnergyCol = new WebInputText(fffEnergyColName + index, false, 1, 0, "In Mev")
        val action = pathOf + "?beamIndex=" + index
        val beamDeleteButton = new FormButton("X", 1, 0, subUrl, action, ButtonType.BtnDefault, beamEnergyButtonColName + index)
        val indexCol = new WebInputHidden(index.toString)

        List(photonEnergyCol, maxDoseRateCol, fffEnergyCol, beamDeleteButton, indexCol)
    }

    def getBeamList(valueMap: ValueMapT): List[Int] = {
        valueMap.keySet.filter { k => k.startsWith(fffEnergyColName) }.map(k => k.replaceAll(".*_", "").toInt).toList.sorted
    }

    private def beamEnergyRows(valueMap: ValueMapT): List[WebRow] = {
        val minSize = 4 // minimum number of beam energies to display
        val beamList = {
            val bl = getBeamList(valueMap)
            if (bl.size < minSize) {
                val max = if (bl.isEmpty) 0 else bl.max
                bl ++ (1 to (minSize - bl.size)).map(b => b + max).toList
            }
            else bl
        }
        beamList.map(b => makeBeamRow(b))
    }

    def fieldList(valueMap: ValueMapT): List[WebRow] = {
        val listA: List[WebRow] = List(
            List(id),
            List(machineTypePK),
            List(multileafCollimatorPK),
            List(configurationDirectory),
            List(epidPK),
            List(institutionPK),
            List(serialNumber, imagingBeam2_5_mv),
            List(onboardImager, sixDimTabletop),
            List(respiratoryManagement, developerMode),
            List(photonEnergyHeader, maxDoseRateHeader, fffEnergyHeader, addBeamEnergyButton))

        val listB: List[WebRow] = beamEnergyRows(valueMap)
        val listC: List[WebRow] = List(List(notes))

        listA ++ listB ++ listC
    }

    val createButtonList: WebRow = List(createButton, cancelButton)
    val editButtonList: WebRow = List(saveButton, cancelButton, deleteButton, maintenanceButton, machinePK)

    private def formCreate(valueMap: ValueMapT) = new WebForm(pathOf, fieldList(valueMap) :+ createButtonList)

    private def formEdit(valueMap: ValueMapT) = new WebForm(pathOf, fieldList(valueMap) :+ editButtonList)

//    private def redirect(response: Response, valueMap: ValueMapT) = {
//        val pk = machinePK.getValOrEmpty(valueMap)
//        val suffix =
//            if (pk.size > 0) { "?" + machinePK.label + "=" + pk }
//            else
//                ""
//    }

    private def emptyId(valueMap: ValueMapT): StyleMapT = {
        val idText = valueMap.get(id.label).get.trim
        val isEmpty = idText.trim.size == 0
        if (isEmpty) {
            Error.make(id, "Id can not be empty")
        }
        else styleNone
    }

    private def updateMachine(machine: Machine): Unit = {
        Machine.query.insertOrUpdate(machine)
    }

    /**
     * Check that id is unique within institution
     */
    private def validateUniqueness(valueMap: ValueMapT): StyleMapT = {
        val instPK = valueMap.get(institutionPK.label).get.toLong
        val machID = valueMap.get(id.label).get.trim
        val machPK = valueMap.get(machinePK.label)

        val machList = {
            val sameIDList = Machine.listMachinesFromInstitution(instPK).filter(m => m.id.equalsIgnoreCase(machID))
            if (machPK.isDefined)
                sameIDList.filter(m => m.machinePK.get != machPK.get.toInt)
            else
                sameIDList
        }

        if (machList.isEmpty) styleNone
        else Error.make(id, "There is already a machine with that name at this institution")
    }

    /**
     * Check that the beam energies specified by the user are valid.  All photon energies must be specified, but
     * max dose and FFF may be blank.  All must be floating point values greater than 0.
     */
    private def validateBeamEnergies(valueMap: ValueMapT, form: WebForm): StyleMapT = {

        val beamList = getBeamList(valueMap)

        def checkEnergy(label: String, required: Boolean): StyleMapT = {
            val input = form.findInput(label).get
            val text = valueMap(label).trim
            stringToDouble(text) match {
                case None if text.nonEmpty => Error.make(input, "Invalid floating point number")
                case None if required => Error.make(input, "Energy is required")
                case None if !required => styleNone
                case Some(d) if (0 >= d) => Error.make(input, "Energy must be greater than 0")
                case Some(d) => styleNone;
            }
        }

        def checkBeam(index: Int): StyleMapT = {
            val phoLabel = photonEnergyColName + index
            val maxLabel = maxDoseRateColName + index
            val fffLabel = fffEnergyColName + index

            // if all values are empty, then ignore it
            if ((valueMap(phoLabel).trim + valueMap(phoLabel).trim + valueMap(phoLabel).trim).isEmpty) styleNone
            else checkEnergy(phoLabel, true) ++ checkEnergy(maxLabel, false) ++ checkEnergy(fffLabel, false)
        }

        beamList.map(i => checkBeam(i)).reduce(_ ++ _)
    }

    private def validateAll(valueMap: ValueMapT, form: WebForm): StyleMapT = emptyId(valueMap) ++ validateUniqueness(valueMap) ++ validateBeamEnergies(valueMap, form)

    /**
     * Save changes made to form.
     */
    private def save(valueMap: ValueMapT, response: Response): Unit = {
        val form = formEdit(valueMap)
        val styleMap = validateAll(valueMap, form)
        if (styleMap.isEmpty) {
            constructMachineFromParameters(valueMap)
            MachineList.redirect(response)
        }
        else {
            form.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    private def updateBeamEnergies(machine: Machine, valueMap: ValueMapT): Unit = {

        def makeBeam(index: Int): Option[MachineBeamEnergy] = {
            val pho = valueMap(photonEnergyColName + index)
            val max = valueMap(maxDoseRateColName + index)
            val fff = valueMap(fffEnergyColName + index)

            val valList = Seq(pho, max, fff)
            val viable = valList.find { x => x.trim.size > 0 }
            if (viable.isDefined)
                Some(new MachineBeamEnergy(None, machine.machinePK.get, stringToDouble(pho), stringToDouble(max), stringToDouble(fff)))
            else
                None
        }

        // The way that the user wants the beam energies
        val requestedBeamList = getBeamList(valueMap).map(b => makeBeam(b)).flatten

        // The beam energies that are in the database now
        val existInDatabase = MachineBeamEnergy.getByMachine(machine.machinePK.get)

        requestedBeamList.diff(existInDatabase).map(be => be.insert)
        existInDatabase.diff(requestedBeamList).map(be => MachineBeamEnergy.delete(be.machineBeamEnergyPK.get))
    }

    /**
     * Create a new machine
     */
    private def constructMachineFromParameters(valueMap: ValueMapT): Unit = {
        val pk: Option[Long] = {
            val e = valueMap.get(machinePK.label)
            if (e.isDefined) Some(e.get.toLong) else None
        }
        val idVal = valueMap.get(id.label).get.trim
        val machineTypePKVal = valueMap.get(machineTypePK.label).get.trim.toLong
        val multilefCollimatorPKVal = valueMap.get(multileafCollimatorPK.label).get.trim.toLong
        val epidPKVal = valueMap.get(epidPK.label).get.trim.toLong
        val institutionPKVal = valueMap.get(institutionPK.label).get.trim.toLong

        val serialNumberVal = {
            if (pk.isDefined) Machine.get(pk.get).get.serialNumber
            else None
        }

        val notesVal = valueMap.get(notes.label).get.trim

        val configDir: Option[String] = {
            if (pk.isDefined) Machine.get(pk.get).get.configurationDirectory
            else None
        }

        val machine = new Machine(pk,
            idVal,
            machineTypePKVal,
            configDir,
            multilefCollimatorPKVal,
            epidPKVal,
            institutionPKVal,
            serialNumberVal,
            valueMap.get(imagingBeam2_5_mv.label).isDefined,
            valueMap.get(onboardImager.label).isDefined,
            valueMap.get(sixDimTabletop.label).isDefined,
            valueMap.get(respiratoryManagement.label).isDefined,
            valueMap.get(developerMode.label).isDefined,
            notesVal)

        if (pk.isDefined) {
            machine.insertOrUpdate
            updateBeamEnergies(machine, valueMap)
        }
        else {
            val newMachine = machine.insert
            updateBeamEnergies(newMachine, valueMap)
        }
    }

    /**
     * Show this when machine asks to create a new machine from machine list.
     */
    private def emptyForm(response: Response) = {
        formCreate(emptyValueMap).setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
    }

    /**
     * Call this when machine has clicked create button.  If everything is ok, then create the new machine,
     * otherwise show the same screen and communicate the error.
     */
    private def create(valueMap: ValueMapT, response: Response) = {
        val form = formCreate(valueMap)

        val styleMap = validateAll(valueMap, form)

        if (styleMap.isEmpty) {
            constructMachineFromParameters(valueMap)
            MachineList.redirect(response)
        }
        else {
            form.setFormResponse(valueMap, styleMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    /**
     * Get the current beam energies for the given machine and convert them into a ValueMapT.  Sort them in order of photon energy.
     */
    private def getBeamEnergyListAsValueMap(machinePK: Long): ValueMapT = {

        val beList = MachineBeamEnergy.getByMachine(machinePK).sortWith((a, b) => a.photonEnergy_MeV.get < b.photonEnergy_MeV.get)

        def sf(d: Option[Double]): String = {
            d match {
                case Some(dbl) => dbl.toString
                case _ => ""
            }
        }

        def beToValueMap(be: MachineBeamEnergy, index: Int): ValueMapT = {
            Map(
                (photonEnergyColName + index, sf(be.photonEnergy_MeV)),
                (maxDoseRateColName + index, sf(be.maxDoseRate_MUperMin)),
                (fffEnergyColName + index, sf(be.fffEnergy_MeV)),
                (beamEnergyButtonColName + index, index.toString))
        }

        beList.zipWithIndex.map(beIndex => beToValueMap(beIndex._1, beIndex._2)).flatten.toMap
    }

    /**
     * Show the edit page by populating the fields with the given machine.
     */
    private def edit(editValueMap: ValueMapT, response: Response): Unit = {
        val pk = editValueMap.get(machinePK.label).get.toString.toLong

        val mach = Machine.get(pk).get

        val boolMap = {
            if (mach.imagingBeam2_5_mv) (imagingBeam2_5_mv.label, "true")
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
            (sixDimTabletop.label, mach.sixDimTabletop.toString),
            (respiratoryManagement.label, mach.respiratoryManagement.toString),
            (developerMode.label, mach.developerMode.toString),
            (notes.label, mach.notes),
            (machinePK.label, pk.toString)) ++ getBeamEnergyListAsValueMap(mach.machinePK.get)

        formEdit(valueMap).setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    }

    private def delete(valueMap: ValueMapT, response: Response): Unit = {
        Machine.delete(valueMap.get(machinePK.label).get.toLong)
        MachineList.redirect(response)
    }

    private def maintRec(valueMap: ValueMapT, response: Response): Unit = {
        val j = valueMap(machinePK.label)
        val path = MaintenanceRecordList.path + "?machinePK=" + valueMap(machinePK.label)
        response.redirectSeeOther(path)
    }

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    private def reload(valueMap: ValueMapT, response: Response): Unit = {
        if (valueMap.get(createButton.label).isDefined) {
            formCreate(valueMap).setFormResponse(valueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
        }
        else {
            formEdit(valueMap).setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
        }
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
        val nameList = Seq(
            photonEnergyColName,
            maxDoseRateColName,
            fffEnergyColName,
            beamEnergyButtonColName).map(n => n + index.toString)

        val vm = valueMap.filter(kv => !(nameList.contains(kv._1)))
        reload(vm, response)
    }

    def buttonIsDeleteBeamEnergyButton(valueMap: ValueMapT): Boolean = {
        !(valueMap.values.filter(k => k.startsWith(beamEnergyButtonColName)).isEmpty)
    }

    /**
     * Determine if the incoming request is to edit.
     */
    private def isEdit(valueMap: ValueMapT): Boolean = valueMap.get(machinePK.label).isDefined

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, cancelButton) => MachineList.redirect(response)
                case _ if buttonIs(valueMap, addBeamEnergyButton) => addBeam(valueMap, response)
                case _ if buttonIsDeleteBeamEnergyButton(valueMap) => deleteBeam(valueMap, response)
                case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
                case _ if buttonIs(valueMap, saveButton) => save(valueMap, response)
                case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
                case _ if buttonIs(valueMap, maintenanceButton) => maintRec(valueMap, response)
                case _ if isEdit(valueMap) => edit(valueMap, response)
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, t)
            }
        }
    }
}
