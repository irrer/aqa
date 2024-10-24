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

import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.web.WebUtil._
import org.aqa.webrun.WebRun
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

object ProcedureUpdate {
  val procedurePKTag = "procedurePK"
}

class ProcedureUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create Procedure"

  private val pageTitleEdit = "Edit Procedure"

  private val name = new WebInputText("Name", 6, 0, "Name of procedure")

  private val version = new WebInputText("Version", 2, 0, "Required.  Usually dot separated integers")

  private val timeout = new WebInputText("Timeout", 2, 0, "Maximum run time in minutes")

  private def webInterfaceList(response: Option[Response]) = WebRun.interfaceChoices.toList.map(name => (name, name))

  private val webInterface = new WebInputSelect("Interface", true, 4, 0, webInterfaceList, false)

  def listSupportingUser(response: Option[Response]) = User.list.map(u => (u.userPK.get.toString, u.id))
  private val supportingUserPK = new WebInputSelect("Author", true, 4, 0, listSupportingUser, true)

  private val notes = new WebInputTextArea("Notes", 6, 0, "")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val procedurePK = new WebInputHidden(ProcedureUpdate.procedurePKTag)

  val fieldList: List[WebRow] = List(List(name), List(version, webInterface), List(timeout, supportingUserPK), List(notes))

  val createButtonList: List[WebRow] = List(List(createButton, cancelButton))

  val editButtonList: List[WebRow] = List(List(saveButton, cancelButton, deleteButton, procedurePK))

  private val formCreate = new WebForm(pathOf, fieldList ++ createButtonList)

  private val formEdit = new WebForm(pathOf, fieldList ++ editButtonList)

  private def redirect(response: Response, valueMap: ValueMapT) = {
    val pk = procedurePK.getValOrEmpty(valueMap)
    val suffix =
      if (pk.size > 0) { "?" + procedurePK.label + "=" + pk }
      else
        ""
    response.redirectSeeOther(pathOf + suffix)
  }

  private def emptyName(valueMap: ValueMapT): StyleMapT = {
    val nameText = valueMap.get(name.label).get.trim
    val isEmpty = nameText.trim.size == 0
    if (isEmpty) Error.make(name, "Name can not be empty")
    else styleNone
  }

  private def validateVersion(valueMap: ValueMapT): StyleMapT = {
    val versionText = valueMap.get(version.label).get.trim
    if (versionText.size == 0) Error.make(version, "Version can not be empty") else styleNone
  }

  private def validateUniqueness(valueMap: ValueMapT): StyleMapT = {
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

  private def validateTimeout(valueMap: ValueMapT): StyleMapT = { // TODO  Should be in nicely formatted time HH:MM:SS.sss
    val timeoutText = valueMap.get(timeout.label).get.trim
    val valOf: Option[Float] = {
      try {
        val f = timeoutText.toFloat
        Some(f)
      } catch {
        case _: Throwable => None
      }
    }
    null match {
      case _ if timeoutText.size == 0 => Error.make(timeout, "Timeout can not be empty")
      case _ if (!valOf.isDefined)    => Error.make(timeout, "Timeout must be a valid floating point number")
      case _ if (valOf.get <= 0)      => Error.make(timeout, "Timeout must be greater than 0.")
      case _                          => styleNone // success
    }
  }

  /**
    * Save changes made to form.
    */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    val errMap = emptyName(valueMap) ++ validateVersion(valueMap) ++ validateUniqueness(valueMap) ++ validateAuthorization(response)
    if (errMap.isEmpty) {
      val procedure = createProcedureFromParameters(valueMap)
      procedure.insertOrUpdate
      logger.info("Updated procedure to: " + procedure.toString)
      ProcedureList.redirect(response)
    } else {
      formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
    * Create a new procedure
    */
  private def createProcedureFromParameters(valueMap: ValueMapT): Procedure = {
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
    formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  /**
    * Only whitelisted users may make changes to procedures.
    */
  private def validateAuthorization(response: Response) = {
    if (WebUtil.userIsWhitelisted(response)) styleNone
    else Error.make(name, "Only system administrators are allowed to create, modify, or delete procedures.")
  }

  /**
    * Call this when procedure has clicked create button.  If everything is ok, then create the new procedure,
    * otherwise show the same screen and communicate the error.
    */
  private def create(valueMap: ValueMapT, response: Response) = {
    val errMap = emptyName(valueMap) ++ validateVersion(valueMap) ++ validateTimeout(valueMap) ++ validateUniqueness(valueMap) ++ validateAuthorization(response)

    if (errMap.isEmpty) {
      val procedure = createProcedureFromParameters(valueMap)
      procedure.insert
      ProcedureList.redirect(response)
    } else {
      formCreate.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
    * Populate the fields with the current values so that the user can edit them.
    */
  private def edit(valueMap: ValueMapT, response: Response) = {

    val procOpt = getReference(valueMap)

    if (procOpt.isDefined) {
      val procedure = procOpt.get
      val valueMap: ValueMapT = Map(
        (procedurePK.label, procedure.procedurePK.get.toString),
        (name.label, procedure.name),
        (version.label, procedure.version),
        (timeout.label, procedure.timeout.toString),
        (supportingUserPK.label, procedure.supportingUserPK.toString),
        (webInterface.label, procedure.webInterface.toString),
        (notes.label, procedure.notes)
      )

      formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
    } else emptyForm(response)
  }

  private def getReference(valueMap: ValueMapT): Option[Procedure] = {
    val value = valueMap.get(ProcedureUpdate.procedurePKTag)
    try {
      if (value.isDefined) {
        val procedure = Procedure.get(value.get.toLong)
        if (procedure.isDefined) procedure else None
      } else
        None
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Perform the delete.
    */
  private def delete(valueMap: ValueMapT, response: Response): Unit = {

    val errMap = validateAuthorization(response)

    if (errMap.isEmpty) {
      val value = valueMap.get(ProcedureUpdate.procedurePKTag)
      if (value.isDefined) {
        Procedure.delete(value.get.toLong)
        ProcedureList.redirect(response)
      }
    } else {
      formEdit.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  /**
    * Determine if the incoming request is to edit an existing procedure.
    */
  private def isEdit(valueMap: ValueMapT): Boolean = valueMap.get(procedurePK.label).isDefined

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => ProcedureList.redirect(response)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton)   => save(valueMap, response)
        case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
        case _ if isEdit(valueMap)                 => edit(valueMap, response)
        case _                                     => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
