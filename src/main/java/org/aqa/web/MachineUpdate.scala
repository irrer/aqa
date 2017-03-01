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

    private val multileafCollimatorPK = new WebInputSelect("Collimator", 6, 0, collimatorName)

    private val epidPK = new WebInputSelect("EPID", 6, 0, epidName)

    private def institutionList() = Institution.list.toList.map(i => (i.institutionPK.get.toString, i.name))

    private val institutionPK = new WebInputSelect("Institution", 6, 0, institutionList)

    private val notes = new WebInputTextArea("Notes", 6, 0, "")

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
    }

    private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

    private val formCreate = new WebForm(pathOf, List(List(id), List(machineTypePK), List(multileafCollimatorPK), List(epidPK), List(institutionPK), List(notes), List(createButton, cancelButton)))

    private val formEdit = new WebForm(pathOf, List(List(id), List(machineTypePK), List(multileafCollimatorPK), List(epidPK), List(institutionPK), List(notes), List(saveButton, cancelButton, deleteButton, machinePK)))

    private def redirect(response: Response, valueMap: ValueMapT) = {
        val pk = machinePK.getValOrEmpty(valueMap)
        val suffix =
            if (pk.size > 0) { "?" + machinePK.label + "=" + pk }
            else
                ""
    }

    private def emptyId(valueMap: ValueMapT): StyleMapT = {
        val idText = valueMap.get(id.label).get.trim
        val isEmpty = idText.trim.size == 0
        if (isEmpty) {
            Error.make(id, "Id can not be empty")
            //formCreate.setFormResponse(Some(valueMap), Some(errMap), pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
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
     * Save changes made to form.
     */
    private def save(valueMap: ValueMapT, response: Response): Unit = {
        val styleMap = emptyId(valueMap) ++ validateUniqueness(valueMap)
        if (styleMap.isEmpty) {
            val machine = createMachineFromParameters(valueMap)
            machine.insertOrUpdate
            MachineList.redirect(response)
        }
        else {
            formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    /**
     * Create a new machine
     */
    private def createMachineFromParameters(valueMap: ValueMapT): Machine = {
        val pk: Option[Long] = {
            val e = valueMap.get(machinePK.label)
            if (e.isDefined) Some(e.get.toLong) else None
        }
        val idVal = valueMap.get(id.label).get.trim
        val machineTypePKVal = valueMap.get(machineTypePK.label).get.trim.toLong
        val multilefCollimatorPKVal = valueMap.get(multileafCollimatorPK.label).get.trim.toLong
        val epidPKVal = valueMap.get(epidPK.label).get.trim.toLong
        val institutionPKVal = valueMap.get(institutionPK.label).get.trim.toLong
        val notesVal = valueMap.get(notes.label).get.trim

        val machine = new Machine(pk, idVal, machineTypePKVal, Some(multilefCollimatorPKVal), Some(epidPKVal), institutionPKVal, notesVal)
        machine
    }

    /**
     * Show this when machine asks to create a new machine from machine list.
     */
    private def emptyForm(response: Response) = {
        formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
    }

    /**
     * Call this when machine has clicked create button.  If everything is ok, then create the new machine,
     * otherwise show the same screen and communicate the error.
     */
    private def create(valueMap: ValueMapT, response: Response) = {
        val styleMap = emptyId(valueMap) ++ validateUniqueness(valueMap)

        if (styleMap.isEmpty) {
            val machine = createMachineFromParameters(valueMap)
            machine.insert
            MachineList.redirect(response)
        }
        else {
            formCreate.setFormResponse(valueMap, styleMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    /**
     * Show the edit page by populating the fields with the given machine.
     */
    private def edit(editValueMap: ValueMapT, response: Response): Unit = {
        val pk = editValueMap.get(machinePK.label).get.toString.toLong

        val mach = Machine.get(pk).get

        val valueMap = Map(
            (id.label, mach.id),
            (machineTypePK.label, mach.machineTypePK.toString),
            (institutionPK.label, mach.institutionPK.toString),
            (notes.label, mach.notes),
            (machinePK.label, pk.toString))

        formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    }

    private def delete(valueMap: ValueMapT, response: Response): Unit = {
        Machine.delete(valueMap.get(machinePK.label).get.toLong)
        MachineList.redirect(response)
    }

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
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
                case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
                case _ if buttonIs(valueMap, saveButton) => save(valueMap, response)
                case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
                case _ if isEdit(valueMap) => edit(valueMap, response)
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                WebUtil.internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }
}
