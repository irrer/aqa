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

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import org.aqa.db.Institution
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
import org.aqa.Crypto
import org.aqa.AnonymizeUtil

object InstitutionUpdate {
  val institutionPKTag = "institutionPK"
}

class InstitutionUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create Institution"

  private val pageTitleEdit = "Edit Institution"

  private val name = new WebInputText("Name", 6, 0, "Commonly used name of institution (required)")

  private val url = new WebInputURL("URL", 6, 0, "Web address (optional)")

  private val description = new WebInputTextArea("Description", 6, 0, "Full name and other identifying information")

  private val notAuthorizedField = new WebPlainText("Not Authorized", 6, 0, <h4>You are not a member of this institution and not authorized to view or modify it.</h4>)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    //val action = InstitutionUpdate.path + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val institutionPK = new WebInputHidden(InstitutionUpdate.institutionPKTag)

  private val formCreate = new WebForm(pathOf, List(List(name), List(url), List(description), List(createButton, cancelButton)))

  private val formEdit = new WebForm(pathOf, List(List(name), List(url), List(description), List(saveButton, cancelButton, deleteButton, institutionPK)))

  private val formNotAuthorized = new WebForm(pathOf, List(List(notAuthorizedField), List(cancelButton)))

  /**
   * Return true if the name is empty.
   */
  private def emptyName(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    val nameText = valueMap.get(name.label).get.trim
    val isEmpty = nameText.trim.size == 0
    if (isEmpty) {
      val errMap = Error.make(name, "Name can not be empty")
      formCreate.setFormResponse(valueMap, errMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
    isEmpty
  }

  private def alreadyExists(valueMap: ValueMapT, response: Response): Boolean = {
    val nameText = valueMap.get(name.label)
    if (nameText.isDefined && Institution.getInstitutionByName(nameText.get).isDefined) {
      val errMap = Error.make(name, "Institution already exists")
      formCreate.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
      true
    } else
      false
  }

  /**
   * Save changes made to form.
   */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    if (emptyName(valueMap, pageTitleEdit, response)) {
      val errMap = Error.make(name, "Instituton name is required")
      formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    } else {
      (createInstitutionFromParameters(valueMap))
      InstitutionList.redirect(response)
    }
  }

  /**
   * Create a new institution
   */
  private def createInstitutionFromParameters(valueMap: ValueMapT): Unit = {
    val institution: Option[Institution] = {
      val e = valueMap.get(InstitutionUpdate.institutionPKTag)
      if (e.isDefined) Institution.get(e.get.toLong) else None
    }

    def update(inst: Institution): Institution = {
      def encrypt(text: String) = AnonymizeUtil.encryptWithNonce(inst.institutionPK.get, text)

      val name_realEnc = encrypt(valueMap.get(name.label).get.trim)
      val url_realEnc = encrypt(valueMap.get(url.label).get.trim)
      val description_realEnc = encrypt(valueMap.get(description.label).get.trim)
      inst.copy(name_real = Some(name_realEnc), url_real = url_realEnc, description_real = description_realEnc)
    }

    if (institution.isDefined) {
      update(institution.get).insertOrUpdate
    } else {
      val inst = (new Institution(None, "", None, "", "")).insert
      val withName = inst.copy(name = AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasPrefixId, inst.institutionPK.get))
      update(withName).insertOrUpdate
    }
  }

  private def emptyForm(response: Response) = {
    formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  private def createRequestIsValid(valueMap: ValueMapT, response: Response): Boolean = {
    0 match {
      case _ if (emptyName(valueMap, pageTitleCreate, response)) => false
      case _ if (alreadyExists(valueMap, response)) => false
      case _ => true
    }
  }

  private def create(valueMap: ValueMapT, response: Response) = {
    if (createRequestIsValid(valueMap, response)) {
      val inst = createInstitutionFromParameters(valueMap)
      InstitutionList.redirect(response)
    }
  }

  private def edit(inst: Institution, response: Response) = {
    val pk = inst.institutionPK.get.toString
    def decrypt(encrypted: String): String = AnonymizeUtil.decryptWithNonce(inst.institutionPK.get, encrypted)

    val valueMap = Map(
      (institutionPK.label, pk),
      (name.label, { if (inst.name_real.isDefined) decrypt(inst.name_real.get) else "" }),
      (url.label, decrypt(inst.url_real)),
      (description.label, decrypt(inst.description_real)))
    val err = deleteErr(Map((institutionPK.label, pk)))
    formEdit.setFormResponse(valueMap, err, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[Institution] = {
    val value = valueMap.get(InstitutionUpdate.institutionPKTag)
    try {
      if (value.isDefined) {
        val inst = Institution.get(value.get.toLong)
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

  private def deleteErr(valueMap: ValueMapT): StyleMapT = {
    val value = valueMap.get(institutionPK.label)
    if (value.isDefined) {
      val instPK = value.get.toLong
      val deleteMessage = Institution.deletable(instPK)
      if (deleteMessage.isDefined) DisableWithTitle.make(deleteButton, "Can not delete: " + deleteMessage.get)
      else styleNone
    } else styleNone
  }

  private def delete(valueMap: ValueMapT, response: Response): Unit = {
    val err = deleteErr(valueMap)

    val value = valueMap.get(InstitutionUpdate.institutionPKTag)
    if (value.isDefined) {
      val instPK = value.get.toLong
      val deleteMessage = Institution.deletable(instPK)
      if (err.isEmpty) {
        Institution.delete(instPK)
        InstitutionList.redirect(response)
      } else {
        val err = Error.make(deleteButton, "Can not delete: " + deleteMessage)
        formEdit.setFormResponse(valueMap, err, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
    } else InstitutionList.redirect(response)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  /**
   * Determine if the incoming request is to edit an existing institution.
   */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
    val value = valueMap.get(InstitutionUpdate.institutionPKTag)
    if (inst.isDefined) {
      true
    } else
      false
  }

  /**
   * Return true if the user is authorized to view and modify this institution.
   */
  private def isAuthorized(request: Request, valueMap: ValueMapT): Boolean = {
    val user = getUser(request)
    val value = valueMap.get(institutionPK.label)
    user.isDefined && value.isDefined && (user.get.institutionPK == value.get.toLong)
  }

  /**
   * The user has attempted to edit or view an institution that they should not be able to.
   */
  private def showNotAuthorized(response: Response) = {
    formNotAuthorized.setFormResponse(emptyValueMap, styleNone, "Not Authorized", response, Status.CLIENT_ERROR_UNAUTHORIZED)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {

      val isWhitelisted = userIsWhitelisted(request)
      val authorized = isAuthorized(request, valueMap) || isWhitelisted

      0 match {
        case _ if buttonIs(valueMap, cancelButton) => InstitutionList.redirect(response)
        case _ if buttonIs(valueMap, createButton) && isWhitelisted => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton) => if (authorized) save(valueMap, response) else showNotAuthorized(response)
        case _ if buttonIs(valueMap, deleteButton) => if (isWhitelisted) delete(valueMap, response) else showNotAuthorized(response)
        case _ if isEdit(valueMap, response) => if (authorized) edit(getReference(valueMap).get, response) else showNotAuthorized(response)
        case _ if isWhitelisted => emptyForm(response)
        case _ => showNotAuthorized(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
