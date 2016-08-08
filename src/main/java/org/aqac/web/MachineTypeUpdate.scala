package org.aqac.web

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
import org.aqac.db.MachineType
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
import org.aqac.Logging._

object MachineTypeUpdate {
    val machineTypePKTag = "machineTypePK"
}

class MachineTypeUpdate extends Restlet with SubUrlAdmin {

    private val pageTitleCreate = "Create MachineType"

    private val pageTitleEdit = "Edit MachineType"

    private val manufacturer = new WebInputText("Manufacturer", 6, 0, "")

    private val model = new WebInputText("Model", 6, 0, "")

    private val version = new WebInputText("Version", 6, 0, "Use when manufacturer and model are not sufficiently specific (optional)")

    private val notes = new WebInputTextArea("Notes", 6, 0, "")

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = pathOf + "?" + name + "=" + name
        new FormButton(name, 1, 0,  subUrl, action, buttonType)
    }

    private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val machineTypePK = new WebInputHidden(MachineTypeUpdate.machineTypePKTag)

    private val formCreate = new WebForm(pathOf, List(List(manufacturer), List(model), List(version), List(notes), List(createButton, cancelButton)))

    private val formEdit = new WebForm(pathOf, List(List(manufacturer), List(model), List(version), List(notes), List(saveButton, cancelButton, deleteButton, machineTypePK)))

    private def emptyManufacturer(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
        val manufacturerText = valueMap.get(manufacturer.label).get.trim
        val isEmpty = manufacturerText.trim.size == 0
        if (isEmpty) {
            val err = Error.make(manufacturer, "Manufacturer can not be empty")
            formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
        isEmpty
    }

    private def emptyModel(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
        val modelText = valueMap.get(model.label).get.trim
        val isEmpty = modelText.trim.size == 0
        if (isEmpty) {
            val err = Error.make(model, "Model can not be empty")
            formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
        isEmpty
    }

    private def alreadyExists(valueMap: Map[String, String], pageTitle: String, response: Response): Boolean = {
        val ae = machineTypeLookup(valueMap).isDefined
        if (ae) {
            val err = Error.make(model, "Machine type already exists")
            formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
        ae
    }

    private def updateMachineType(inst: MachineType): Unit = {
        MachineType.query.insertOrUpdate(inst)
    }

    private def machineTypeLookup(valueMap: Map[String, String]): Option[MachineType] = {
        MachineType.get(manufacturer.getValOrEmpty(valueMap).trim, model.getValOrEmpty(valueMap).trim, version.getValOrEmpty(valueMap).trim)
    }

    private def okToSaveAs(valueMap: Map[String, String], pageTitle: String, response: Response): Boolean = {
        val mt = machineTypeLookup(valueMap)
        if (mt.isDefined && (mt.get.machineTypePK.get != valueMap.get(MachineTypeUpdate.machineTypePKTag).get.toInt)) {
            val err = Error.make(model, "Machine type already exists")
            formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
            false
        }
        else true
    }

    private def okToSave(valueMap: Map[String, String], pageTitle: String, response: Response): Boolean = {
        0 match {
            case _ if !fieldsAreValid(valueMap, pageTitleEdit, response) => false
            case _ if !okToSaveAs(valueMap, pageTitleEdit, response) => false
            case _ => true
        }
    }

    /**
     * Save changes made to form.
     */
    private def save(valueMap: Map[String, String], pageTitle: String, response: Response): Unit = {
        if (okToSave(valueMap, pageTitle, response)) {
            (createMachineTypeFromParameters(valueMap)).insertOrUpdate
            MachineTypeList.redirect(response)
        }
    }

    /**
     * Create a new machineType
     */
    private def createMachineTypeFromParameters(valueMap: Map[String, String]): MachineType = {
        val machineTypePK: Option[Long] = {
            val e = valueMap.get(MachineTypeUpdate.machineTypePKTag)
            if (e.isDefined) Some(e.get.toLong) else None
        }

        new MachineType(machineTypePK,
            manufacturer.getValOrEmpty(valueMap).trim,
            model.getValOrEmpty(valueMap).trim,
            version.getValOrEmpty(valueMap).trim,
            notes.getValOrEmpty(valueMap).trim)
    }

    private def emptyForm(response: Response) = {
        formCreate.setFormResponse(Map[String, String](), styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
    }

    private def fieldsAreValid(valueMap: Map[String, String], pageTitle: String, response: Response): Boolean = {
        0 match {
            case _ if emptyManufacturer(valueMap, pageTitle, response) => false
            case _ if emptyModel(valueMap, pageTitle, response) => false
            case _ => true
        }
    }

    private def createRequestIsValid(valueMap: Map[String, String], response: Response): Boolean = {
        0 match {
            case _ if !fieldsAreValid(valueMap, pageTitleCreate, response) => false
            case _ if (alreadyExists(valueMap, pageTitleCreate, response)) => false
            case _ => true
        }
    }

    private def create(valueMap: Map[String, String], response: Response) = {
        if (createRequestIsValid(valueMap, response)) {
            val inst = createMachineTypeFromParameters(valueMap)
            inst.insert
            MachineTypeList.redirect(response)
        }
    }

    private def edit(inst: MachineType, response: Response) = {
        val valueMap = Map((machineTypePK.label, inst.machineTypePK.get.toString), (manufacturer.label, inst.manufacturer), (model.label, inst.model), (version.label, inst.version), (notes.label, inst.notes))
        formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    }

    private def getReference(valueMap: Map[String, String]): Option[MachineType] = {
        val value = valueMap.get(MachineTypeUpdate.machineTypePKTag)
        try {
            if (value.isDefined) {
                val inst = MachineType.get(value.get.toLong)
                if (inst.isDefined) {
                    inst
                }
                else
                    None
            }
            else
                None
        }
        catch {
            case _: Throwable => None
        }
    }

    private def isDelete(valueMap: Map[String, String], response: Response): Boolean = {
        if (buttonIs(valueMap, deleteButton)) {
            val value = valueMap.get(MachineTypeUpdate.machineTypePKTag)
            if (value.isDefined) {
                MachineType.delete(value.get.toLong)
                MachineTypeList.redirect(response)
                true
            }
            else
                false
        }
        else
            false

    }

    private def buttonIs(valueMap: Map[String, String], button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    /**
     * Determine if the incoming request is to edit an existing machineType.
     */
    private def isEdit(valueMap: Map[String, String], response: Response): Boolean = {
        val inst = getReference(valueMap)
        val value = valueMap.get(MachineTypeUpdate.machineTypePKTag)
        if (inst.isDefined) {
            edit(inst.get, response)
            true
        }
        else
            false
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, cancelButton) => MachineTypeList.redirect(response)
                case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
                case _ if buttonIs(valueMap, saveButton) => save(valueMap, pageTitleEdit, response)
                case _ if isDelete(valueMap, response) => Nil
                case _ if isEdit(valueMap, response) => Nil
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
