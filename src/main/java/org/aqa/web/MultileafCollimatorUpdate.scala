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

import org.aqa.db.Machine
import org.aqa.db.MultileafCollimator
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

object MultileafCollimatorUpdate {
  val multileafCollimatorPKTag = "multileafCollimatorPK"
}

class MultileafCollimatorUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create MultileafCollimator"

  private val pageTitleEdit = "Edit MultileafCollimator"

  private val manufacturer = new WebInputText("Manufacturer", 6, 0, "")

  private val model = new WebInputText("Model", 6, 0, "")

  private val version = new WebInputText("Version", 6, 0, "Use when manufacturer and model are not sufficiently specific (optional)")

  private val notes = new WebInputTextArea("Notes", 6, 0, "")

  private val outerLeafPairCount = new WebInputText("Outer Leaf Count", 3, 0, "No. of opposing outer pairs, must be 0 or greater.")
  private val innerLeafPairCount = new WebInputText("Inner Leaf Count", 3, 0, "No. of opposing inner pairs, must be 0 or greater.")

  private val outerLeafWidth_cm = new WebInputText("Outer Leaf Width (cm)", 3, 0, "Outer leaf width in cm")
  private val innerLeafWidth_cm = new WebInputText("Inner Leaf Width (cm)", 3, 0, "Inner leaf width in cm")

  private val leafTravelDistance_cm = new WebInputText("Leaf travel distance (cm)", 3, 1, "Distance that leaf can travel in cm")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val multileafCollimatorPK = new WebInputHidden(MultileafCollimatorUpdate.multileafCollimatorPKTag)

  val fieldList: List[WebRow] = List(
    List(manufacturer),
    List(model),
    List(version),
    List(outerLeafPairCount, innerLeafPairCount),
    List(outerLeafWidth_cm, innerLeafWidth_cm),
    List(leafTravelDistance_cm),
    List(notes)
  )

  val createButtonList: List[WebRow] = List(List(createButton, cancelButton))
  val editButtonList: List[WebRow] = List(List(saveButton, cancelButton, deleteButton, multileafCollimatorPK))

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
    val ae = multileafCollimatorLookup(valueMap).isDefined
    if (ae) {
      val err = Error.make(model, "Machine type already exists")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
    ae
  }

  private def multileafCollimatorLookup(valueMap: ValueMapT): Option[MultileafCollimator] = {
    MultileafCollimator.get(manufacturer.getValOrEmpty(valueMap).trim, model.getValOrEmpty(valueMap).trim, version.getValOrEmpty(valueMap).trim)
  }

  /**
    * Only whitelisted users may make changes to multileaf collimators.
    */
  private def validateAuthorization(valueMap: ValueMapT, create: Boolean, response: Response): Boolean = {
    if (WebUtil.userIsWhitelisted(response)) true
    else {
      val err = Error.make(model, "Only system administrators are allowed to create, modify, or delete multileaf collimators.")
      if (create) formCreate.setFormResponse(valueMap, err, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
      else formEdit.setFormResponse(valueMap, err, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
      false
    }
  }

  private def okToSaveAs(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    val mt = multileafCollimatorLookup(valueMap)
    if (mt.isDefined && (mt.get.multileafCollimatorPK.get != valueMap.get(MultileafCollimatorUpdate.multileafCollimatorPKTag).get.toInt)) {
      val err = Error.make(model, "Machine type already exists")
      formCreate.setFormResponse(valueMap, err, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
      false
    } else true
  }

  private def okToSave(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    0 match {
      case _ if !fieldsAreValid(valueMap, pageTitleEdit, response) => false
      case _ if !okToSaveAs(valueMap, pageTitleEdit, response)     => false
      case _                                                       => validateAuthorization(valueMap, false, response)
    }
  }

  /**
    * Save changes made to form.
    */
  private def save(valueMap: ValueMapT, pageTitle: String, response: Response): Unit = {
    if (okToSave(valueMap, pageTitle, response)) {
      (createMultileafCollimatorFromParameters(valueMap)).insertOrUpdate
      MultileafCollimatorList.redirect(response)
    }
  }

  /**
    * Create a new multileafCollimator
    */
  private def createMultileafCollimatorFromParameters(valueMap: ValueMapT): MultileafCollimator = {
    val multileafCollimatorPK: Option[Long] = {
      val e = valueMap.get(MultileafCollimatorUpdate.multileafCollimatorPKTag)
      if (e.isDefined) Some(e.get.toLong) else None
    }

    new MultileafCollimator(
      multileafCollimatorPK,
      manufacturer.getValOrEmpty(valueMap).trim,
      model.getValOrEmpty(valueMap).trim,
      version.getValOrEmpty(valueMap).trim,
      outerLeafPairCount.getValOrEmpty(valueMap).trim.toInt,
      innerLeafPairCount.getValOrEmpty(valueMap).trim.toInt,
      outerLeafWidth_cm.getValOrEmpty(valueMap).trim.toDouble,
      innerLeafWidth_cm.getValOrEmpty(valueMap).trim.toDouble,
      leafTravelDistance_cm.getValOrEmpty(valueMap).trim.toDouble,
      notes.getValOrEmpty(valueMap).trim
    )
  }

  private def emptyForm(response: Response) = {
    formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  private def fieldsAreValid(valueMap: ValueMapT, pageTitle: String, response: Response): Boolean = {
    0 match {
      case _ if emptyManufacturer(valueMap, pageTitle, response)                    => false
      case _ if emptyModel(valueMap, pageTitle, response)                           => false
      case _ if !isPositiveInt(valueMap, pageTitle, response, outerLeafPairCount)   => false
      case _ if !isPositiveInt(valueMap, pageTitle, response, innerLeafPairCount)   => false
      case _ if !isPositiveDouble(valueMap, pageTitle, response, outerLeafWidth_cm) => false
      case _ if !isPositiveDouble(valueMap, pageTitle, response, innerLeafWidth_cm) => false
      case _ if !isDouble(valueMap, pageTitle, response, leafTravelDistance_cm)     => false
      case _                                                                        => true
    }
  }

  private def createRequestIsValid(valueMap: ValueMapT, response: Response): Boolean = {
    0 match {
      case _ if !fieldsAreValid(valueMap, pageTitleCreate, response) => false
      case _ if (alreadyExists(valueMap, pageTitleCreate, response)) => false
      case _                                                         => validateAuthorization(valueMap, true, response)
    }
  }

  private def create(valueMap: ValueMapT, response: Response) = {
    if (createRequestIsValid(valueMap, response)) {
      val inst = createMultileafCollimatorFromParameters(valueMap)
      inst.insert
      MultileafCollimatorList.redirect(response)
    }
  }

  private def edit(inst: MultileafCollimator, response: Response) = {
    val valueMap = Map(
      (multileafCollimatorPK.label, inst.multileafCollimatorPK.get.toString),
      (manufacturer.label, inst.manufacturer),
      (model.label, inst.model),
      (version.label, inst.version),
      (outerLeafPairCount.label, inst.outerLeafPairCount.toString),
      (innerLeafPairCount.label, inst.innerLeafPairCount.toString),
      (outerLeafWidth_cm.label, inst.outerLeafWidth_cm.toString),
      (innerLeafWidth_cm.label, inst.innerLeafWidth_cm.toString),
      (leafTravelDistance_cm.label, inst.leafTravelDistance_cm.toString),
      (notes.label, inst.notes)
    )
    formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[MultileafCollimator] = {
    val value = valueMap.get(MultileafCollimatorUpdate.multileafCollimatorPKTag)
    try {
      if (value.isDefined) {
        val inst = MultileafCollimator.get(value.get.toLong)
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
    if (validateAuthorization(valueMap, false, response)) {
      val value = valueMap.get(MultileafCollimatorUpdate.multileafCollimatorPKTag)
      if (value.isDefined) {
        val multileafCollimatorPK = value.get.toLong
        val machCount = Machine.listMachinesWithCollimator(multileafCollimatorPK).size
        if (machCount == 0) {
          MultileafCollimator.delete(value.get.toLong)
          MultileafCollimatorList.redirect(response)
        } else {
          val err = Error.make(model, "This multileaf collimator type is in use by " + machCount + " machines and can not be deleted.")
          formCreate.setFormResponse(valueMap, err, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
        }
      }
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  /**
    * Determine if the incoming request is to edit an existing multileafCollimator.
    */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
    val value = valueMap.get(MultileafCollimatorUpdate.multileafCollimatorPKTag)
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
        case _ if buttonIs(valueMap, cancelButton) => MultileafCollimatorList.redirect(response)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton)   => save(valueMap, pageTitleEdit, response)
        case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
        case _ if isEdit(valueMap, response)       => Nil
        case _                                     => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
