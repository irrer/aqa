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

import com.pixelmed.dicom.AttributeFactory
import edu.umro.DicomDict.TagByName
import org.aqa.Logging
import org.aqa.db.DicomAnonymous
import org.aqa.db.PatientProcedure
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil.ValueMapT
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

object PatientProcedureUpdate {
  val patientPKTag = "patientPK"
}

class PatientProcedureUpdate extends Restlet with SubUrlAdmin with Logging {

  private val pageTitleCreate = "Create"

  private val pageTitleEdit = "Edit"

  private val patientId = new WebInputText("Patient ID", showLabel = true, 2, 0, "Patient ID", aqaAlias = true)

  private val procedure = new WebInputSelect(label = "Procedure", showLabel = false, col = 3, offset = 0, selectList = procedureList, aqaAlias = false)

  private val active = new WebInputCheckbox(label = "Active", showLabel = true, title = Some("Check to indicate that data is actively being created for this patient."), col = 3, offset = 0)

  /**
    * Get the list of prodedures that the user may choose from.
    */
  //noinspection ScalaUnusedSymbol
  private def procedureList(response: Option[Response]): Seq[(String, String)] = {
    // @formatter:off
    val procList = Procedure.list.                                               // get list of all procedures from database
      map(p => if (p.name.contains("BB by")) p.copy(name = "Daily QA") else p).  // change both Daily QA procedures to commonly known name.
      map(p => if (p.name.contains("LOC")) p.copy(name = "LOC") else p).         // change both LOC procedures to commonly known name.
      groupBy(_.name).                                                           // group by procedure name
      map(np => np._2.maxBy(_.version)).                                         // choose the latest version of each
      toSeq.                                                                     // convert to expected container type
      map(p => (p.procedurePK.get.toString, p.name))                             // convert to key + text pair, using procedurePK as key.
    // @formatter:on
    procList
  }

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", primary = true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", primary = true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", primary = false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)
  private val patientProcedurePK = new WebInputHidden("patientProcedurePK")

  private val formCreate = new WebForm(pathOf, List(List(patientId), List(procedure, active), List(createButton, cancelButton)))

  private val formEdit = new WebForm(pathOf, List(List(patientId), List(procedure, active), List(saveButton, cancelButton, deleteButton, patientProcedurePK)))

  /**
    * Get the list of known DicomAnonymous values for PatientID for this institution. This
    * includes ALL patients from all DICOM files uploaded.
    *
    * @param valueMap Enables access to institution ID.
    * @return List of DicomAnonymous values.
    */
  private def getKnownPatientIdList(valueMap: ValueMapT): Seq[DicomAnonymous] = {
    val institutionPK = getUser(valueMap).get.institutionPK
    DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagByName.PatientID))
  }

  /**
    * Construct a row from valueMap parameters
    */
  private def constructFromParameters(valueMap: ValueMapT): PatientProcedure = {

    val dicomAnonPatId: DicomAnonymous = {
      val clearText = valueMap(patientId.label).trim
      val institutionPK = getUser(valueMap).get.institutionPK
      val attr = AttributeFactory.newAttribute(TagByName.PatientID)
      attr.addValue(clearText)

      val dicomAnon = getKnownPatientIdList(valueMap).find(da => da.originalValue.equals(clearText)) match {
        case Some(da) => da // a DicomAnonymous value already exists.
        case _        => DicomAnonymous.insert(institutionPK, attr) // No such DicomAnonymous value, so make a new one
      }
      dicomAnon
    }

    val patientProcedure = PatientProcedure(
      patientProcedurePK = if (valueMap.contains(patientProcedurePK.label)) Some(valueMap(patientProcedurePK.label).toLong) else None,
      dicomAnonPatId.dicomAnonymousPK.get,
      institutionPK = getUser(valueMap).get.institutionPK,
      procedurePK = valueMap(procedure.label).toLong,
      active = valueMap.contains(active.label)
    )
    patientProcedure
  }

  /**
    * Show this when user asks to create a new row.
    */
  private def emptyForm(response: Response): Unit = {
    formCreate.setFormResponse(Map((active.label, true.toString)), styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  /**
    * Do not allow the patient ID to be empty.  White space does not count.
    *
    * @param valueMap Parameters given by user.
    * @return List of errors, which may be empty.
    */
  private def validateNonEmptyPatientId(valueMap: ValueMapT): StyleMapT = {
    if (valueMap(patientId.label).trim.isEmpty) {
      Error.make(patientId, inputTitle = "The patient ID is not allowed to be empty.")
    } else
      styleNone
  }

  /**
    * Make sure that the patient ID is only assigned to one procedure.
    *
    * @param valueMap Parameters given by user.
    * @return List of errors, which may be empty.
    */
  private def validateUnique(valueMap: ValueMapT): StyleMapT = {
    val ppPK = if (valueMap.contains(patientProcedurePK.label)) Some(valueMap(patientProcedurePK.label).toLong) else None
    val institutionPK = getUser(valueMap).get.institutionPK
    val existing = PatientProcedure.listExtended(institutionPK)
    // get list of other rows, excluding this one
    val others = if (ppPK.isDefined) existing.filterNot(_.patientProcedure.patientProcedurePK.get == ppPK.get) else existing

    // patient ID in clear text being proposed
    val patId = valueMap(patientId.label).trim

    // list of other patient ID's in this list, in clear text
    val otherPatientIdList = others.map(ip => ip.dicomAnonymous.originalValue)

    if (otherPatientIdList.contains(patId))
      Error.make(patientId, inputTitle = "This patient ID is already assigned to a procedure.")
    else
      styleNone
  }

  /**
    * Perform all validations.
    *
    * @param valueMap Parameters given by user.
    * @return List of errors, which may be empty.
    */
  def validate(valueMap: ValueMapT): StyleMapT = validateNonEmptyPatientId(valueMap) ++ validateUnique(valueMap)

  /**
    * Save changes made to form.
    */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    val errMap = validate(valueMap)
    if (errMap.isEmpty) {
      if (isAuthorized(valueMap, response)) {
        val patientProcedure = constructFromParameters(valueMap)
        patientProcedure.insertOrUpdate()
        AnonymousTranslate.clearCache(WebUtil.getUser(valueMap).get.institutionPK)
        PatientProcedureList.redirect(response)
      }
    } else {
      formEdit.setFormResponse(valueMap, errMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
    * Fill the fields with the values from the database row.
    * @param valueMap Contains patientProcedurePK.
    * @param response Put form data here.
    */
  def edit(valueMap: ValueMapT, response: Response): Unit = {
    val pp = PatientProcedure.get(valueMap(patientProcedurePK.label).toLong).get

    val vm: ValueMapT = Map(
      (patientId.label, DicomAnonymous.get(pp.dicomAnonymousPK).get.value),
      (patientProcedurePK.label, pp.patientProcedurePK.get.toString),
      (procedure.label, pp.procedurePK.toString),
      (active.label, pp.active.toString)
    )

    formEdit.setFormResponse(vm, styleNone, "Edit Patient Procedure", response, Status.SUCCESS_OK)
  }

  def isEdit(valueMap: ValueMapT): Boolean = {
    valueMap.contains(patientProcedurePK.label)
  }

  /**
    * Call this when user has clicked create button.  If everything is ok, then create the new user,
    * otherwise show the same screen and communicate the error.
    */
  private def create(valueMap: ValueMapT, response: Response): Unit = {
    val errMap = validate(valueMap)
    if (errMap.isEmpty) {
      val patientProcedure = constructFromParameters(valueMap)
      patientProcedure.insertOrUpdate()
      AnonymousTranslate.clearCache(WebUtil.getUser(valueMap).get.institutionPK)
      PatientProcedureList.redirect(response)
    } else {
      formCreate.setFormResponse(valueMap, errMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
    * Return true if the user is authorized to change or delete an existing row.  They must be
    * from the same institution as the row's.
    *
    * @param valueMap Parameters
    * @param response Contains user id.
    *
    * @return True if user is from the same institution as the row.
    */

  private def isAuthorized(valueMap: ValueMapT, response: Response): Boolean = {
    val user = getUser(response.getRequest).get
    val ppPK = valueMap(patientProcedurePK.label).toLong
    val pp = PatientProcedure.get(ppPK).get
    val ok = pp.institutionPK == user.institutionPK
    if (!ok) {
      // @formatter:off
      val notAuthorizedField = new WebPlainText("Authorization Failure", showLabel = true, col = 3, offset = 0, _ => {
        <h4>You must be from the same institution to make changes.</h4>
      })
      // @formatter:on
      val formNotAuthorized = new WebForm(pathOf, List(List(notAuthorizedField), List(cancelButton)))
      formNotAuthorized.setFormResponse(emptyValueMap, styleNone, "Not Authorized", response, Status.CLIENT_ERROR_UNAUTHORIZED)
    }
    ok
  }

  private def delete(valueMap: ValueMapT, response: Response): Unit = {
    if (isAuthorized(valueMap, response)) {
      PatientProcedure.delete(valueMap(patientProcedurePK.label).toLong)
      PatientProcedureList.redirect(response)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => PatientProcedureList.redirect(response)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton)   => save(valueMap, response)
        case _ if buttonIs(valueMap, deleteButton) => delete(valueMap, response)
        case _ if isEdit(valueMap)                 => edit(valueMap, response)
        case _                                     => emptyForm(response)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
