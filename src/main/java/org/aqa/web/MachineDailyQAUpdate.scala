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

import org.aqa.Config
import org.aqa.db.Machine
import org.aqa.db.MachineDailyQA
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.Elem

/**
  * Provide administrative user interface for setting the Daily QA limits on a per-machine basis.
  */

object MachineDailyQAUpdate {
  def machineReference(machinePK: Long): String = { "/" + SubUrl.admin + "/" + "MachineDailyQAUpdate" + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  /**
    * Redirect to the URL for this Restlet.
    */
  def redirect(machinePK: Long, response: Response): Unit = {
    response.redirectSeeOther(machineReference(machinePK))
  }
}

class MachineDailyQAUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleEdit = "Edit Daily QA Parameters"

  private val pageHeader = new WebPlainText("Daily QA Parameters", false, 6, 0, formatHeader)

  private val passLimit = new WebInputText("Pass Limit (mm)", 2, 0, "")

  private val warningLimit = new WebInputText("Warning Limit (mm)", 2, 0, "")

  private val requireXRayOffset = new WebInputCheckbox(
    "Require X-Ray Offset",
    showLabel = true,
    Some("Check if the machine supports X-Ray offset, and it will be required to NOT have the value of exactly 0, 0, -500"),
    col = 2,
    offset = 0
  )

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action: String = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, _ => action, buttonType, name, Some(""))
  }

  private val saveButton = makeButton("Save", primary = true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Use Defaults", primary = false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)

  private val machineDailyQAPK = new WebInputHidden(MachineUpdate.machinePKTag)

  private val formEdit = new WebForm(pathOf, List(List(pageHeader), List(passLimit), List(warningLimit), List(requireXRayOffset), List(saveButton, cancelButton, deleteButton, machineDailyQAPK)))

  private def formatHeader(valueMap: ValueMapT): Elem = {
    try {
      val machine = Machine.get(valueMap(machineDailyQAPK.label).toLong).get
      <h2>Daily QA Parameters for <span aqaalias="">{machine.id}</span></h2>
    } catch {
      case _: Throwable => <h2>Daily QA Parameters</h2>
    }
  }

  private def isPosDbl(text: String): Boolean = {
    try {
      val d = text.trim.toDouble
      d > 0
    } catch {
      case _: Throwable => false
    }
  }

  /** Return an error if the passLimit field is not valid. */
  private def validatePassMax(valueMap: ValueMapT): StyleMapT = {
    0 match {
      case _ if valueMap(passLimit.label).trim.isEmpty => Error.make(warningLimit, "Warning can not be empty")
      case _ if !isPosDbl(valueMap(passLimit.label))   => Error.make(warningLimit, "Warning must be a floating point value greater than zero.")
      case _                                           => styleNone
    }
  }

  /** Return an error if the warningLimit field is not valid. */
  private def validateWarningMax(valueMap: ValueMapT): StyleMapT = {
    0 match {
      case _ if valueMap(warningLimit.label).trim.isEmpty => Error.make(passLimit, "Pass can not be empty")
      case _ if !isPosDbl(valueMap(warningLimit.label))   => Error.make(warningLimit, "Pass must be a floating point value greater than zero.")
      case _                                              => styleNone
    }
  }

  /**
    * Make sure that warningLimit is >= passLimit.
    */
  private def validatePassAndWarning(valueMap: ValueMapT): StyleMapT = {
    try {
      val p = valueMap(passLimit.label).trim.toDouble
      val w = valueMap(warningLimit.label).trim.toDouble

      if (w >= p) styleNone else Error.make(passLimit, "Warning must be less than or equal to Pass.")
    } catch {
      case _: Throwable => Error.make(passLimit, "Invalid values for one or both of Pass and Warn.")
    }
  }

  /**
    * Determine if the referenced machine is valid.  Failing this would be truly weird, but it could happen if someone were
    * trying to hack the system.
    */
  private def validateMachine(valueMap: ValueMapT): StyleMapT = {
    try {
      val machPK = valueMap(machineDailyQAPK.label).toLong
      if (Machine.get(machPK).isDefined) styleNone else Error.make(passLimit, "Machine does not exist.")
    } catch {
      case _: Throwable => Error.make(passLimit, "No valid machine specified.")
    }
  }

  /**
    * Determine if user is authorized. Assume that the machine is valid.  User must be from same
    * institution as machine or be whitelisted.
    */
  private def validateAuthorization(valueMap: ValueMapT): StyleMapT = {
    if (userIsWhitelisted(valueMap)) styleNone
    else {
      val machPK = valueMap(machineDailyQAPK.label).trim.toLong
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
    val machDailyQAPK = valueMap(machineDailyQAPK.label).trim.toLong

    val mdq = new MachineDailyQA(
      Some(machDailyQAPK), // this is the same as the <code>Machine.machinePK</code>.
      passLimit.getValOrEmpty(valueMap).trim.toDouble,
      warningLimit.getValOrEmpty(valueMap).trim.toDouble,
      valueMap.contains(requireXRayOffset.label)
    )
    mdq
  }

  /**
    * Save changes made to form editing an existing machine type.
    */
  private def saveEdits(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = validate(valueMap)
    if (styleMap.isEmpty) {
      createMachineDailyQAFromParameters(valueMap).insertOrUpdate()

      // MachineUpdate.redirect(machPK, response)
      MachineDailyQAList.redirect(response)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  /**
    * Populate a value map from existing values in the database.
    */
  private def populateValueMap(machDailyQA: MachineDailyQA): ValueMapT = {
    Map(
      (machineDailyQAPK.label, machDailyQA.machineDailyQAPK.get.toString),
      (passLimit.label, machDailyQA.passLimit_mm.toString),
      (warningLimit.label, machDailyQA.warningLimit_mm.toString),
      (requireXRayOffset.label, machDailyQA.requireXRayOffset.toString)
    )
  }

  private def defaultValueMap(valueMap: ValueMapT): ValueMapT = {
    Map(
      (machineDailyQAPK.label, valueMap(machineDailyQAPK.label)),
      (passLimit.label, Config.DailyQAPassLimit_mm.toString),
      (warningLimit.label, Config.DailyQAWarningLimit_mm.toString),
      (requireXRayOffset.label, "off")
    )
  }

  private def emptyForm(valueMap: ValueMapT, response: Response): Unit = {
    val valMap = getReference(valueMap) match {
      case Some(machDailyQA) => populateValueMap(machDailyQA)
      case _                 => defaultValueMap(valueMap)

    }

    formEdit.setFormResponse(valMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def edit(inst: MachineDailyQA, response: Response): Unit = {
    val valueMap = Map(
      (machineDailyQAPK.label, inst.machineDailyQAPK.get.toString),
      (passLimit.label, inst.passLimit_mm.toString),
      (warningLimit.label, inst.warningLimit_mm.toString),
      (requireXRayOffset.label, if (inst.requireXRayOffset) "on" else "off")
    )
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

  private def delete(valueMap: ValueMapT, response: Response): Unit = {
    val auth = validateAuthorization(valueMap)
    if (auth.isEmpty) {
      val dailyQAPK = valueMap.get(machineDailyQAPK.label)
      if (dailyQAPK.isDefined) {
        MachineDailyQA.delete(dailyQAPK.get.toLong)
        MachineDailyQAList.redirect(response)
      }
    } else {
      formEdit.setFormResponse(valueMap, auth, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  /**
    * Determine if the incoming request is to edit an existing machineType.
    */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
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
        case _ if buttonIs(valueMap, cancelButton) => MachineDailyQAList.redirect(response) //  MachineUpdate.redirect(valueMap(machineDailyQAPK.label).toLong, response)
        case _ if buttonIs(valueMap, saveButton)   => saveEdits(valueMap, response)
        case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
        case _ if isEdit(valueMap, response)       =>
        case _                                     => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
