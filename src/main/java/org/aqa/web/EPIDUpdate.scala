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
import org.aqa.db.EPID
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

object EPIDUpdate {
  val epidPKTag = "epidPK"
}

class EPIDUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create EPID"

  private val pageTitleEdit = "Edit EPID"

  private val manufacturer = new WebInputText("Manufacturer", 6, 0, "")

  private val model = new WebInputText("Model", 6, 0, "")

  private val hardwareVersion = new WebInputText("HW Version", 6, 0, "Hardware hardwareVersion (optional)")

  private val notes = new WebInputTextArea("Notes", 6, 0, "")

  private val pixelCountX = new WebInputText("Width in pixels", 3, 0, "Width of image in pixels")
  private val pixelCountY = new WebInputText("Height in pixels", 3, 0, "Height of image in pixels")

  private val width_cm = new WebInputText("Imaging width (cm)", 3, 0, "Imaging width in cm")
  private val height_cm = new WebInputText("Imaging height (cm)", 3, 0, "Imaging height in cm")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val epidPK = new WebInputHidden(EPIDUpdate.epidPKTag)

  val fieldList: List[WebRow] = List(List(manufacturer), List(model), List(hardwareVersion),
    List(pixelCountX, pixelCountY),
    List(width_cm, height_cm),
    List(notes))

  val createButtonList: List[WebRow] = List(List(createButton, cancelButton))
  val editButtonList: List[WebRow] = List(List(saveButton, cancelButton, deleteButton, epidPK))

  private val formCreate = new WebForm(pathOf, fieldList ++ createButtonList)

  private val formEdit = new WebForm(pathOf, fieldList ++ editButtonList)

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

  private def isPositiveInt(valueMap: ValueMapT, pageTitle: String, response: Response, input: IsInput): Boolean = {
    if (input.getInt(valueMap).isDefined) true
    else {
      val err = Error.make(input, "Must be an integer 0 or greater")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
      false
    }
  }

  private def isDouble(valueMap: ValueMapT, pageTitle: String, response: Response, input: IsInput): Boolean = {
    if (input.getDouble(valueMap).isDefined) true
    else {
      val err = Error.make(input, "Must be a valid floating point number")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
      false
    }
  }

  private def isPositiveDouble(valueMap: ValueMapT, pageTitle: String, response: Response, input: IsInput): Boolean = {
    val d = input.getDouble(valueMap)
    if (d.isDefined && (d.get >= 0)) true
    else {
      val err = Error.make(input, "Must be a valid floating point number 0 or greater")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
      false
    }
  }

  private def alreadyExists(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    val ae = epidLookup(valueMap).isDefined
    if (ae) {
      val err = Error.make(model, "Machine type already exists")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
    ae
  }

  private def updateEPID(inst: EPID): Unit = {
    EPID.query.insertOrUpdate(inst)
  }

  private def epidLookup(valueMap: ValueMapT): Option[EPID] = {
    EPID.get(manufacturer.getValOrEmpty(valueMap).trim, model.getValOrEmpty(valueMap).trim, hardwareVersion.getValOrEmpty(valueMap).trim)
  }

  private def okToSaveAs(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    val mt = epidLookup(valueMap)
    if (mt.isDefined && (mt.get.epidPK.get != valueMap.get(EPIDUpdate.epidPKTag).get.toInt)) {
      val err = Error.make(model, "Machine type already exists")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
      false
    } else true
  }

  private def okToSave(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    0 match {
      case _ if !fieldsAreValid(valueMap, pageTitleEdit, response) => false
      case _ if !okToSaveAs(valueMap, pageTitleEdit, response) => false
      case _ => true
    }
  }

  /**
   * Save changes made to form.
   */
  private def save(valueMap: ValueMapT, pageTitle: String, response: Response): Unit = {
    if (okToSave(valueMap, pageTitle, response)) {
      (createEPIDFromParameters(valueMap)).insertOrUpdate
      EPIDList.redirect(response)
    }
  }

  /**
   * Create a new epid
   */
  private def createEPIDFromParameters(valueMap: ValueMapT): EPID = {
    val epidPK: Option[Long] = {
      val e = valueMap.get(EPIDUpdate.epidPKTag)
      if (e.isDefined) Some(e.get.toLong) else None
    }

    new EPID(
      epidPK,
      manufacturer.getValOrEmpty(valueMap).trim,
      model.getValOrEmpty(valueMap).trim,
      hardwareVersion.getValOrEmpty(valueMap).trim,
      pixelCountX.getValOrEmpty(valueMap).trim.toInt,
      pixelCountY.getValOrEmpty(valueMap).trim.toInt,
      width_cm.getValOrEmpty(valueMap).trim.toDouble,
      height_cm.getValOrEmpty(valueMap).trim.toDouble,
      notes.getValOrEmpty(valueMap).trim)
  }

  private def emptyForm(response: Response) = {
    formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  private def fieldsAreValid(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    0 match {
      case _ if emptyManufacturer(valueMap, pageTitle, response) => false
      case _ if emptyModel(valueMap, pageTitle, response) => false
      case _ if !isPositiveInt(valueMap, pageTitle, response, pixelCountX) => false
      case _ if !isPositiveInt(valueMap, pageTitle, response, pixelCountY) => false
      case _ if !isPositiveDouble(valueMap, pageTitle, response, width_cm) => false
      case _ if !isPositiveDouble(valueMap, pageTitle, response, height_cm) => false
      case _ => true
    }
  }

  private def createRequestIsValid(valueMap: ValueMapT, response: Response): Boolean = {
    0 match {
      case _ if !fieldsAreValid(valueMap, pageTitleCreate, response) => false
      case _ if (alreadyExists(valueMap, pageTitleCreate, response)) => false
      case _ => true
    }
  }

  private def create(valueMap: ValueMapT, response: Response) = {
    if (createRequestIsValid(valueMap, response)) {
      val inst = createEPIDFromParameters(valueMap)
      inst.insert
      EPIDList.redirect(response)
    }
  }

  private def edit(inst: EPID, response: Response) = {
    val valueMap = Map(
      (epidPK.label, inst.epidPK.get.toString),
      (manufacturer.label, inst.manufacturer),
      (model.label, inst.model),
      (hardwareVersion.label, inst.hardwareVersion),

      (pixelCountX.label, inst.pixelCountX.toString),
      (pixelCountY.label, inst.pixelCountY.toString),

      (width_cm.label, inst.width_cm.toString),
      (height_cm.label, inst.height_cm.toString),

      (notes.label, inst.notes))
    formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[EPID] = {
    val value = valueMap.get(EPIDUpdate.epidPKTag)
    try {
      if (value.isDefined) {
        val inst = EPID.get(value.get.toLong)
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
    if (buttonIs(valueMap, deleteButton)) {
      val value = valueMap.get(EPIDUpdate.epidPKTag)
      if (value.isDefined) {
        EPID.delete(value.get.toLong)
        EPIDList.redirect(response)
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
   * Determine if the incoming request is to edit an existing epid.
   */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
    val value = valueMap.get(EPIDUpdate.epidPKTag)
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
        case _ if buttonIs(valueMap, cancelButton) => EPIDList.redirect(response)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton) => save(valueMap, pageTitleEdit, response)
        case _ if isDelete(valueMap, response) => Nil
        case _ if isEdit(valueMap, response) => Nil
        case _ => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
