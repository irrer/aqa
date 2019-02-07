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
import org.aqa.db.MaintenanceRecord
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
import org.aqa.db.Machine
import java.sql.Timestamp
import org.aqa.Config

object MaintenanceRecordUpdate {
  val maintenanceRecordPKTag = "maintenanceRecordPK"
}

class MaintenanceRecordUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create Maintenance Record"

  private val pageTitleEdit = "Edit Maintenance Record"

  private val maintenanceRecordPK = new WebInputHidden(MaintenanceRecordUpdate.maintenanceRecordPKTag)

  private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private val dateTime = new WebInputDateTime("Date / Time", 6, 0, "Format: Month/Day/Year Hour:Minute")

  private def getOutputReference(valueMap: ValueMapT): Elem = {
    try {
      val pk = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag).get.toLong
      val outputPK = MaintenanceRecord.get(pk).get.outputPK.get
      val url = "/view/ViewOutput?outputPK=" + outputPK
      <a href={ url }>View Originating Output</a>
    } catch {
      case t: Throwable => {
        <div>No related output</div>
      }
    }
  }

  private val outputReference = new WebPlainText("RelatedOutput", false, 6, 0, getOutputReference _)

  //private val user = new WebInputText("User", 6, 0, "")

  private val summary = new WebInputText("Summary", 6, 0, "")

  private val category = new WebInputSelect("Category", 6, 0, (response: Option[Response]) => Config.MaintenanceCategoryList.map(m => (m.Name, m.Name)))

  private val description = new WebInputTextArea("Description", 6, 0, "")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  val fieldList: List[WebRow] = List(
    List(summary),
    List(category),
    List(description),
    List(outputReference),
    List(dateTime))

  val createButtonList: WebRow = List(createButton, cancelButton, machinePK)
  val editButtonList: WebRow = List(saveButton, cancelButton, deleteButton, machinePK, maintenanceRecordPK)

  private val formCreate = new WebForm(pathOf, fieldList :+ createButtonList)

  private val formEdit = new WebForm(pathOf, fieldList :+ editButtonList)

  private def redirectToList(response: Response, valueMap: ValueMapT): Unit = {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MaintenanceRecordList.getClass.getName) + "?" + MachineUpdate.machinePKTag + "=" + valueMap(MachineUpdate.machinePKTag)
    response.redirectSeeOther(path)
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
    val ok = WebUtil.userIsWhitelisted(request) ||
      {
        getUser(request) match {
          case Some(user) => user.institutionPK == institutionPK
          case _ => false
        }
      }
    ok
  }

  private def emptySummary(valueMap: ValueMapT): StyleMapT = {
    val summaryText = valueMap.get(summary.label).get.trim
    val isEmpty = summaryText.trim.size == 0
    if (isEmpty) Error.make(summary, summary.label + " can not be empty")
    else styleNone
  }

  private def validateAuthentication(valueMap: ValueMapT, request: Request): StyleMapT = {

    val machPK = {
      if (valueMap.get(machinePK.label).isDefined) {
        valueMap.get(machinePK.label).get.toLong
      } else {
        (MaintenanceRecord.get(valueMap.get(maintenanceRecordPK.label).get.toLong).get).machinePK
      }
    }
    val instPK = Machine.get(machPK).get.institutionPK

    if (isAuthorized(request, instPK)) styleNone
    else Error.make(summary, "You are not allowed to modify maintenance records for machines from other institutions.")
  }

  private def dateIsInvalid(valueMap: ValueMapT): StyleMapT = {
    if (dateTime.validateDateTime(valueMap.get(dateTime.label).get).isDefined) styleNone
    else Error.make(dateTime, dateTime.label + " is invalid.  Expected format: MM/DD/YYYY hh:mm")
  }

  private def updateMaintenanceRecord(inst: MaintenanceRecord): Unit = {
    MaintenanceRecord.query.insertOrUpdate(inst)
  }

  private def checkFields(valueMap: ValueMapT, request: Request): StyleMapT = validateAuthentication(valueMap, request) ++ emptySummary(valueMap) ++ dateIsInvalid(valueMap)

  /**
   * Create a new maintenanceRecord
   */
  private def createMaintenanceRecordFromParameters(valueMap: ValueMapT, request: Request): MaintenanceRecord = {
    val mrPK: Option[Long] = {
      val e = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag)
      if (e.isDefined) Some(e.get.toLong) else None
    }

    val machPK = machinePK.getValOrEmpty(valueMap).trim.toLong
    val dt = new Timestamp(dateTime.validateDateTime(dateTime.getValOrEmpty(valueMap)).get.getTime)
    val uPK = getUser(request).get.userPK.get
    val cat = valueMap.get(category.label).get

    new MaintenanceRecord(
      mrPK,
      cat,
      machPK,
      dt,
      uPK,
      None, // outputPK
      summary.getValOrEmpty(valueMap).trim,
      description.getValOrEmpty(valueMap).trim)

  }

  private def emptyForm(response: Response, valueMap: ValueMapT) = {
    val vm = Map(
      (machinePK.label, valueMap(machinePK.label)),
      (dateTime.label, dateTime.dateTimeFormat.format(System.currentTimeMillis)))
    formCreate.setFormResponse(vm, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  /**
   * Save changes made to form.
   */
  private def save(valueMap: ValueMapT, pageTitle: String, response: Response): Unit = {
    val styleMap = checkFields(valueMap, response.getRequest)
    if (styleMap.isEmpty) {
      (createMaintenanceRecordFromParameters(valueMap, response.getRequest)).insertOrUpdate
      redirectToList(response, valueMap)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  private def create(valueMap: ValueMapT, pageTitle: String, response: Response) = {
    val styleMap = checkFields(valueMap, response.getRequest)
    if (styleMap.isEmpty) {
      createMaintenanceRecordFromParameters(valueMap, response.getRequest).insert
      redirectToList(response, valueMap)
    } else formCreate.setFormResponse(valueMap, styleMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  private def edit(inst: MaintenanceRecord, response: Response) = {
    val valueMap = Map(
      (maintenanceRecordPK.label, inst.maintenanceRecordPK.get.toString),
      (category.label, inst.category),
      (machinePK.label, inst.machinePK.toString),
      (dateTime.label, dateTime.dateTimeFormat.format(inst.creationTime)),
      (summary.label, inst.summary),
      (description.label, inst.description))
    formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[MaintenanceRecord] = {
    val value = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag)
    try {
      if (value.isDefined) {
        val inst = MaintenanceRecord.get(value.get.toLong)
        if (inst.isDefined) {
          inst
        } else
          None
      } else
        None
    } catch {
      case _: Throwable => None
    }
  }

  private def isDelete(valueMap: ValueMapT, response: Response): Boolean = {
    val styleMap = validateAuthentication(valueMap, response.getRequest)
    if (buttonIs(valueMap, deleteButton)) {
      val value = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag)
      if (value.isDefined) {
        if (styleMap.isEmpty) {
          MaintenanceRecord.delete(value.get.toLong)
          redirectToList(response, valueMap)
        } else {
          formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.SUCCESS_OK)
        }
        true
      } else
        false
    } else
      false

  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  /**
   * Determine if the incoming request is to edit an existing MaintenanceRecord.
   */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
    val value = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag)
    if (inst.isDefined) {
      edit(inst.get, response)
      true
    } else
      false
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => redirectToList(response, valueMap)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, pageTitleEdit, response)
        case _ if buttonIs(valueMap, saveButton) => save(valueMap, pageTitleEdit, response)
        case _ if isDelete(valueMap, response) => Nil
        case _ if isEdit(valueMap, response) => Nil
        case _ => emptyForm(response, valueMap)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}