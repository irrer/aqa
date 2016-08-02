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
import org.aqac.db.Procedure
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
import org.aqac.db.Institution.InstitutionTable
import org.aqac.db.Institution
import org.aqac.db.User

object ProcedureUpdate {
    val procedurePKTag = "procedurePK"
}

class ProcedureUpdate extends Restlet with SubUrlAdmin {

    private val pageTitleCreate = "Create Procedure"

    private val pageTitleEdit = "Edit Procedure"

    private val name = new WebInputText("Name", 6, 0, "Name of procedure")

    private val version = new WebInputText("Version", 6, 0, "Required.  Usually dot separated integers")

    private def webInterfaceList() = List(("WinstonLutz_1", "WinstonLutz_1"), ("MaxLeafGap_1", "MaxLeafGap_1")) // TODO should get list of interfaces

    private val webInterface = new WebInputSelect("Interface", 6, 0, webInterfaceList)

    private val timeout = new WebInputText("Timeout", 6, 0, "Maximum run time in minutes")

    def directory = new WebPlainText("Directory", 6, 0, "For executable and test data") // TODO

    def listSupportingUser() = User.list.map(u => (u.userPK.get.toString, u.fullName))
    private val supportingUserPK = new WebInputSelect("Author", 6, 0, listSupportingUser)

    private val notes = new WebInputTextArea("Notes", 6, 0, "")

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        new FormButton(name, 1, 0,  subUrl, pathOf, buttonType)
    }

    private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
    private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
    private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val procedurePK = new WebInputHidden(ProcedureUpdate.procedurePKTag)

    val fieldList: List[WebRow] = List(List(name), List(version), List(timeout), List(webInterface), List(supportingUserPK), List(notes), List(directory))

    val createButtonList: List[WebRow] = List(List(createButton, cancelButton))

    val editButtonList: List[WebRow] = List(List(saveButton, cancelButton, deleteButton, procedurePK))

    private val formCreate = new WebForm(pathOf, fieldList ++ createButtonList)

    private val formEdit = new WebForm(pathOf, fieldList ++ editButtonList)

    private def redirect(response: Response, valueMap: Map[String, String]) = {
        val pk = procedurePK.getValOrEmpty(valueMap)
        val suffix =
            if (pk.size > 0) { "?" + procedurePK.label + "=" + pk }
            else
                ""
        response.redirectSeeOther(pathOf + suffix)
    }

    private def emptyName(valueMap: Map[String, String]): Map[String, Style] = {
        val nameText = valueMap.get(name.label).get.trim
        val isEmpty = nameText.trim.size == 0
        if (isEmpty) Error.make(name, "Name can not be empty")
        else styleNone
    }

    private def validateVersion(valueMap: Map[String, String]): Map[String, Style] = {
        val versionText = valueMap.get(version.label).get.trim
        if (versionText.size == 0) Error.make(version, "Version can not be empty") else styleNone
    }

    private def validateUniqueness(valueMap: Map[String, String]): Map[String, Style] = {
        val pk: Long = {
            val text = valueMap.get(procedurePK.label)
            if (text.isDefined) text.get.toLong
            else -1
        }
        val nameText = valueMap.get(name.label).get.trim
        val versionText = valueMap.get(version.label).get.trim
        
        val existingList = Procedure.list.filter(p => p.name.equalsIgnoreCase(nameText) && (p.version.equalsIgnoreCase(versionText)) && (p.procedurePK.get != pk))
        
        if (existingList.isEmpty)
            styleNone
        else
            Error.make(name, "There is already a procedure with that name and version of " + nameText + " " + versionText)
    }

    private def validateTimeout(valueMap: Map[String, String]): Map[String, Style] = { // TODO  Should be in nicely formatted time HH:MM:SS.sss
        val timeoutText = valueMap.get(timeout.label).get.trim
        val valOf: Option[Float] = {
            try {
                val f = timeoutText.toFloat
                Some(f)
            }
            catch {
                case _: Throwable => None
            }
        }
        null match {
            case _ if timeoutText.size == 0 => Error.make(timeout, "Timeout can not be empty")
            case _ if (!valOf.isDefined) => Error.make(timeout, "Timeout must be a valid floating point number")
            case _ if (valOf.get <= 0) => Error.make(timeout, "Timeout must be greater than 0.")
            case _ => styleNone // success
        }
    }

    private def updateProcedure(procedure: Procedure): Unit = {
        Procedure.query.insertOrUpdate(procedure)
    }

    /**
     * Save changes made to form.
     */
    private def save(valueMap: Map[String, String], response: Response): Unit = {
        val errMap = emptyName(valueMap) ++ validateVersion(valueMap) ++ validateUniqueness(valueMap)
        if (errMap.isEmpty) {
            (createProcedureFromParameters(valueMap)).insertOrUpdate
            ProcedureList.redirect(response)
        }
        else {
            formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    /**
     * Create a new procedure
     */
    private def createProcedureFromParameters(valueMap: Map[String, String]): Procedure = {
        val procedurePK: Option[Long] = {
            val e = valueMap.get(ProcedureUpdate.procedurePKTag)
            if (e.isDefined) Some(e.get.toLong) else None
        }
        val nameText = valueMap.get(name.label).get.trim
        val versionText = valueMap.get(version.label).get.trim
        val timeoutText = valueMap.get(timeout.label).get.trim
        val createdDate = new java.sql.Date(System.currentTimeMillis)
        val supUserPK = valueMap.get(supportingUserPK.label).get.toLong
        val webInterfaceText = valueMap.get(webInterface.label).get.trim
        val notesText = valueMap.get(notes.label).get.trim

        new Procedure(procedurePK, nameText, versionText, timeoutText.toFloat, createdDate, supUserPK, webInterfaceText, notesText)
    }

    /**
     * Show this when procedure asks to create a new procedure from procedure list.
     */
    private def emptyForm(response: Response) = {
        formCreate.setFormResponse(Map[String, String](), styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
    }

    /**
     * Call this when procedure has clicked create button.  If everything is ok, then create the new procedure,
     * otherwise show the same screen and communicate the error.
     */
    private def create(valueMap: Map[String, String], response: Response) = {
        val errMap = emptyName(valueMap) ++ validateVersion(valueMap) ++ validateTimeout(valueMap) ++ validateUniqueness(valueMap)

        if (errMap.isEmpty) {
            val procedure = createProcedureFromParameters(valueMap)
            procedure.insert
            ProcedureList.redirect(response)
        }
        else {
            formCreate.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
    }

    private def edit(valueMap: Map[String, String], response: Response) = {

        val procOpt = getReference(valueMap)

        if (procOpt.isDefined) {
            val procedure = procOpt.get
            val valueMap: Map[String, String] = Map(
                (procedurePK.label, procedure.procedurePK.get.toString),
                (name.label, procedure.name),
                (version.label, procedure.version),
                (timeout.label, procedure.timeout.toString),
                (supportingUserPK.label, procedure.supportingUserPK.toString),
                (notes.label, procedure.notes))

            formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
        }
        else emptyForm(response)
    }

    private def getReference(valueMap: Map[String, String]): Option[Procedure] = {
        val value = valueMap.get(ProcedureUpdate.procedurePKTag)
        try {
            if (value.isDefined) {
                val procedure = Procedure.get(value.get.toLong)
                if (procedure.isDefined) procedure else None
            }
            else
                None
        }
        catch {
            case _: Throwable => None
        }
    }

    private def delete(valueMap: Map[String, String], response: Response): Unit = {
        val value = valueMap.get(ProcedureUpdate.procedurePKTag)
        if (value.isDefined) {
            Procedure.delete(value.get.toLong)
            ProcedureList.redirect(response)
        }
    }

    private def buttonIs(valueMap: Map[String, String], button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    /**
     * Determine if the incoming request is to edit an existing procedure.
     */
    private def isEdit(valueMap: Map[String, String]): Boolean = valueMap.get(procedurePK.label).isDefined

    override def handle(request: Request, response: Response): Unit = {
        val valueMap = getValueMap(request)
        try {
            0 match {
                case _ if buttonIs(valueMap, cancelButton) => ProcedureList.redirect(response)
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