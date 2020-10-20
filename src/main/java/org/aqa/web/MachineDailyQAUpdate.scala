package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import org.aqa.db.MachineDailyQA
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.aqa.db.Machine
import org.aqa.Config

/**
 * Provide administrative user interface for setting the Daily QA limits on a per-machine basis.
 */

object MachineDailyQAUpdate {
  def machineReference(machinePK: Long) = { "/" + SubUrl.admin + "/" + "MachineDailyQAUpdate" + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  /**
   * Redirect to the URL for this Restlet.
   */
  def redirect(machinePK: Long, response: Response) = {
    response.redirectSeeOther(machineReference(machinePK))
  }
}

class MachineDailyQAUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleEdit = "Edit Daily QA Parameters"

  private val pageHeader = new WebPlainText("Daily QA Parameters", false, 6, 0, formatHeader)

  private val passLimit = new WebInputText("Pass Limit (mm)", 2, 0, "")

  private val warningLimit = new WebInputText("Warning Limit (mm)", 2, 0, "")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action: String = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, (_) => action, buttonType, name, Some(""))
  }

  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val machineDailyQAPK = new WebInputHidden(MachineUpdate.machinePKTag)

  private val formEdit = new WebForm(pathOf, List(List(pageHeader), List(passLimit), List(warningLimit), List(saveButton, cancelButton, deleteButton, machineDailyQAPK)))

  private def formatHeader(valueMap: ValueMapT): Elem = {
    try {
      val machine = Machine.get(valueMap.get(machineDailyQAPK.label).get.toLong).get
      <h2>Daily QA Parameters for <span aqaalias="">{ machine.id }</span></h2>
    } catch {
      case t: Throwable => <h2>Daily QA Parameters</h2>
    }
  }

  private def isPosDbl(text: String): Boolean = {
    try {
      val d = text.trim.toDouble
      d > 0
    } catch {
      case t: Throwable => false
    }
  }

  /** Return an error if the passLimit field is not valid. */
  private def validatePassMax(valueMap: ValueMapT): StyleMapT = {
    0 match {
      case _ if (valueMap.get(passLimit.label).get.trim.isEmpty) => Error.make(warningLimit, "Warning can not be empty")
      case _ if (!isPosDbl(valueMap.get(passLimit.label).get)) => Error.make(warningLimit, "Warning must be a floating point value greater than zero.")
      case _ => styleNone
    }
  }

  /** Return an error if the warningLimit field is not valid. */
  private def validateWarningMax(valueMap: ValueMapT): StyleMapT = {
    0 match {
      case _ if (valueMap.get(warningLimit.label).get.trim.isEmpty) => Error.make(passLimit, "Pass can not be empty")
      case _ if (!isPosDbl(valueMap.get(warningLimit.label).get)) => Error.make(warningLimit, "Pass must be a floating point value greater than zero.")
      case _ => styleNone
    }
  }

  /**
   * Make sure that warningLimit is >= passLimit.
   */
  private def validatePassAndWarning(valueMap: ValueMapT): StyleMapT = {
    try {
      val p = valueMap.get(passLimit.label).get.trim.toDouble
      val w = valueMap.get(warningLimit.label).get.trim.toDouble

      if (w >= p) styleNone else Error.make(passLimit, "Warning must be less than or equal to Pass.")
    } catch {
      case t: Throwable => Error.make(passLimit, "Invalid values for one or both of Pass and Warn.")
    }
  }

  /**
   * Determine if the referenced machine is valid.  Failing this would be truly weird, but it could happen if someone were
   * trying to hack the system.
   */
  private def validateMachine(valueMap: ValueMapT): StyleMapT = {
    try {
      val machPK = valueMap.get(machineDailyQAPK.label).get.toLong
      if (Machine.get(machPK).isDefined) styleNone else Error.make(passLimit, "Machine does not exist.")
    } catch {
      case t: Throwable => Error.make(passLimit, "No valid machine specified.")
    }
  }

  /**
   * Determine if user is authorized. Assume that the machine is valid.  User must be from same
   * institution as machine or be whitelisted.
   */
  private def validateAuthorization(valueMap: ValueMapT): StyleMapT = {
    if (userIsWhitelisted(valueMap)) styleNone
    else {
      val machPK = valueMap.get(machineDailyQAPK.label).get.trim.toLong
      val machine = Machine.get(machPK)

      if (machine.isEmpty) styleNone
      else {
        val user = WebUtil.getUser(valueMap)
        if (user.isDefined && (user.get.institutionPK == machine.get.institutionPK)) styleNone
        else Error.make(passLimit, "User is not authorized to edit Daily QA parameters for this machine.")
      }
    }

  }

  private def validate(valueMap: ValueMapT): StyleMapT = {
    validatePassMax(valueMap) ++
      validateWarningMax(valueMap) ++
      validatePassAndWarning(valueMap) ++
      validateMachine(valueMap) ++
      validateAuthorization(valueMap)
  }

  /**
   * Create a new MachineDailyQA from the value map
   */
  private def createMachineDailyQAFromParameters(valueMap: ValueMapT): MachineDailyQA = {
    // make the machDailyQAPK value the same as the machinePK value
    val machDailyQAPK = valueMap.get(machineDailyQAPK.label).get.trim.toLong

    val mdq = new MachineDailyQA(
      Some(machDailyQAPK), // this is the same as the <code>Machine.machinePK</code>.
      passLimit.getValOrEmpty(valueMap).trim.toDouble,
      warningLimit.getValOrEmpty(valueMap).trim.toDouble)
    mdq
  }

  /**
   * Save changes made to form editing an existing machine type.
   */
  private def saveEdits(valueMap: ValueMapT, pageTitle: String, response: Response): Unit = {
    val styleMap = validate(valueMap)
    if (styleMap.isEmpty) {
      (createMachineDailyQAFromParameters(valueMap)).insertOrUpdate
      val machPK = valueMap.get(machineDailyQAPK.label).get.trim.toLong

      MachineUpdate.redirect(machPK, response)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  /**
   * Populate a value map from existing values in the database.
   */
  private def populateValueMap(machDailyQA: MachineDailyQA): ValueMapT = {
    Map(
      (machineDailyQAPK.label, machDailyQA.machineDailyQAPK.get.toString),
      (passLimit.label, machDailyQA.passLimit_mm.toString),
      (warningLimit.label, machDailyQA.warningLimit_mm.toString))
  }

  private def defaultValueMap(valueMap: ValueMapT): ValueMapT = {
    Map(
      (machineDailyQAPK.label, valueMap(machineDailyQAPK.label)),
      (passLimit.label, Config.DailyQAPassLimit_mm.toString),
      (warningLimit.label, Config.DailyQAWarningLimit_mm.toString))
  }

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    val valMap = getReference(valueMap) match {
      case Some(machDailyQA) => populateValueMap(machDailyQA)
      case _ => defaultValueMap(valueMap)

    }

    formEdit.setFormResponse(valMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def edit(inst: MachineDailyQA, response: Response) = {
    val valueMap = Map((machineDailyQAPK.label, inst.machineDailyQAPK.get.toString), (passLimit.label, inst.passLimit_mm.toString), (warningLimit.label, inst.warningLimit_mm.toString))
    formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[MachineDailyQA] = {
    val value = valueMap.get(machineDailyQAPK.label)
    try {
      if (value.isDefined) {
        val inst = MachineDailyQA.get(value.get.toLong)
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

  private def delete(valueMap: ValueMapT, response: Response) = {
    val auth = validateAuthorization(valueMap)
    if (auth.isEmpty) {
      val dailyQAPK = valueMap.get(machineDailyQAPK.label)
      if (dailyQAPK.isDefined) {
        MachineDailyQA.delete(dailyQAPK.get.toLong)
        val machPK = valueMap.get(machineDailyQAPK.label).get.toLong
        MachineUpdate.redirect(machPK, response)
      }
    } else {
      formEdit.setFormResponse(valueMap, auth, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  /**
   * Determine if the incoming request is to edit an existing machineType.
   */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
    val value = valueMap.get(machineDailyQAPK.label)
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
        case _ if buttonIs(valueMap, cancelButton) => MachineUpdate.redirect(valueMap(machineDailyQAPK.label).toLong, response)
        case _ if buttonIs(valueMap, saveButton) => saveEdits(valueMap, pageTitleEdit, response)
        case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
        case _ if isEdit(valueMap, response) => Nil
        case _ => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
