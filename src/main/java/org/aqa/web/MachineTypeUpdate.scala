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
import slick.lifted.TableQuery
import org.aqa.db.MachineType
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

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
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val machineTypePK = new WebInputHidden(MachineTypeUpdate.machineTypePKTag)

  private val formCreate = new WebForm(pathOf, List(List(manufacturer), List(model), List(version), List(notes), List(createButton, cancelButton)))

  private val formEdit = new WebForm(pathOf, List(List(manufacturer), List(model), List(version), List(notes), List(saveButton, cancelButton, deleteButton, machineTypePK)))

  /** Return an error if the manufacturer field is not valid. */
  private def validateManufacturer(valueMap: ValueMapT): StyleMapT = {
    if (valueMap.get(manufacturer.label).get.trim.isEmpty) Error.make(manufacturer, "Manufacturer can not be empty")
    else styleNone
  }

  /** Return an error if the model field is not valid. */
  private def validateModel(valueMap: ValueMapT): StyleMapT = {
    if (valueMap.get(model.label).get.trim.isEmpty) Error.make(model, "Model can not be empty")
    else styleNone
  }

  /** Return an error if the machine type already exists. */
  private def validateExistence(valueMap: ValueMapT): StyleMapT = {
    if (machineTypeLookup(valueMap).isDefined) Error.make(model, "Machine type already exists")
    else styleNone
  }

  private def machineTypeLookup(valueMap: ValueMapT): Option[MachineType] = {
    MachineType.get(manufacturer.getValOrEmpty(valueMap).trim, model.getValOrEmpty(valueMap).trim, version.getValOrEmpty(valueMap).trim)
  }

  private val alreadyExistsStyle = {
    val msg = "A machine type with that manufacturer, model, and version already exists"
    Error.make(manufacturer, msg) ++ Error.make(model, msg) ++ Error.make(version, msg)
  }

  /**
   * Only whitelisted users may make changes to procedures.
   */
  private def validateAuthorization(response: Response) = {
    if (WebUtil.userIsWhitelisted(response)) styleNone
    else Error.make(model, "Only system administrators are allowed to create, modify, or delete machine types.")
  }

  private def okToSaveEdited(valueMap: ValueMapT): StyleMapT = {
    val mt = machineTypeLookup(valueMap)
    if (mt.isDefined && (mt.get.machineTypePK.get != valueMap.get(MachineTypeUpdate.machineTypePKTag).get.toInt)) alreadyExistsStyle
    else styleNone
  }

  /**
   * Save changes made to form editing an existing machine type.
   */
  private def saveEdits(valueMap: ValueMapT, pageTitle: String, response: Response): Unit = {
    val styleMap = validateManufacturer(valueMap) ++ validateModel(valueMap) ++ okToSaveEdited(valueMap) ++ validateAuthorization(response)
    if (styleMap.isEmpty) {
      (createMachineTypeFromParameters(valueMap)).insertOrUpdate
      MachineTypeList.redirect(response)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  /**
   * Create a new machineType
   */
  private def createMachineTypeFromParameters(valueMap: ValueMapT): MachineType = {
    val machineTypePK: Option[Long] = {
      val e = valueMap.get(MachineTypeUpdate.machineTypePKTag)
      if (e.isDefined) Some(e.get.toLong) else None
    }

    new MachineType(
      machineTypePK,
      manufacturer.getValOrEmpty(valueMap).trim,
      model.getValOrEmpty(valueMap).trim,
      version.getValOrEmpty(valueMap).trim,
      notes.getValOrEmpty(valueMap).trim)
  }

  private def emptyForm(response: Response) = {
    formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  private def okToCreate(valueMap: ValueMapT, response: Response): StyleMapT = {
    if (machineTypeLookup(valueMap).isDefined) alreadyExistsStyle
    else validateAuthorization(response)
  }

  private def create(valueMap: ValueMapT, response: Response) = {
    val styleMap = validateManufacturer(valueMap) ++ validateModel(valueMap) ++ okToCreate(valueMap, response)
    if (styleMap.isEmpty) {
      val inst = createMachineTypeFromParameters(valueMap)
      inst.insert
      MachineTypeList.redirect(response)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  private def edit(inst: MachineType, response: Response) = {
    val valueMap = Map((machineTypePK.label, inst.machineTypePK.get.toString), (manufacturer.label, inst.manufacturer), (model.label, inst.model), (version.label, inst.version), (notes.label, inst.notes))
    formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[MachineType] = {
    val value = valueMap.get(MachineTypeUpdate.machineTypePKTag)
    try {
      if (value.isDefined) {
        val inst = MachineType.get(value.get.toLong)
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
    val auth = validateAuthorization(response)
    if (auth.isEmpty) {
      val value = valueMap.get(MachineTypeUpdate.machineTypePKTag)
      if (value.isDefined) {
        MachineType.delete(value.get.toLong)
        MachineTypeList.redirect(response)
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
    val value = valueMap.get(MachineTypeUpdate.machineTypePKTag)
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
        case _ if buttonIs(valueMap, cancelButton) => MachineTypeList.redirect(response)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton) => saveEdits(valueMap, pageTitleEdit, response)
        case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
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
