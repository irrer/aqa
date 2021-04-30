package org.aqa.web

import com.pixelmed.dicom.AttributeFactory
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.DicomAnonymous
import org.aqa.db.PatientProcedure
import org.aqa.db.Procedure
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil._
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
    * Get the list of institutions that the user may choose from.
    */
  //noinspection ScalaUnusedSymbol
  private def procedureList(response: Option[Response]): Seq[(String, String)] = {
    // @formatter:off
    val procList = Procedure.list.                                               // get list of all procedures from database
      map(p => if (p.name.contains("BB by")) p.copy(name = "Daily QA") else p).  // change both Daily QA procedures to commonly known name.
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
    * Construct a row from valueMap parameters
    */
  private def constructFromParameters(valueMap: ValueMapT): PatientProcedure = {

    val patId = {
      val clearText = valueMap(patientId.label)
      val institutionPK = getUser(valueMap).get.institutionPK
      val attr = AttributeFactory.newAttribute(TagByName.PatientID)
      attr.addValue(clearText)

      val anonValue = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagByName.PatientID))

      val dicomAnon = anonValue.find(da => da.originalValue.equals(clearText)) match {
        case Some(da) => da
        case _        => DicomAnonymous.insert(institutionPK, attr)
      }
      Trace.trace(dicomAnon.dicomAnonymousPK.get + " :: " + dicomAnon.value + " :: " + dicomAnon.originalValue)
      dicomAnon.value
    }

    val patientProcedure = PatientProcedure(
      patientProcedurePK = if (valueMap.contains(patientProcedurePK.label)) Some(valueMap(patientProcedurePK.label).toLong) else None,
      patientId = patId,
      institutionPK = getUser(valueMap).get.institutionPK,
      procedurePK = valueMap(procedure.label).toLong,
      active = valueMap.contains(active.label)
    )
    patientProcedure
  }

  /**
    * Show this when user asks to create a new user from user list.
    */
  private def emptyForm(response: Response): Unit = {
    formCreate.setFormResponse(Map((active.label, true.toString)), styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  private def validateNonEmptyPatientId(valueMap: ValueMapT): StyleMapT = {
    if (valueMap(patientId.label).trim.isEmpty) {
      Error.make(patientId, inputTitle = "The patient ID is not allowed to be empty.")
    } else
      styleNone
  }

  private def validateUnique(valueMap: ValueMapT): StyleMapT = {
    val ppPK = if (valueMap.contains(patientProcedurePK.label)) Some(valueMap(patientProcedurePK.label).toLong) else None
    styleNone // TODO
  }

  /**
    * Save changes made to form.
    */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    val errMap = validateNonEmptyPatientId(valueMap) ++ validateUnique(valueMap)
    if (errMap.isEmpty) {
      if (isAuthorized(valueMap, response)) {
        val patientProcedure = constructFromParameters(valueMap)
        patientProcedure.insertOrUpdate()
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
      (patientId.label, pp.patientId),
      (patientProcedurePK.label, pp.patientProcedurePK.get.toString),
      (procedure.label, pp.procedurePK.toString),
      (active.label, pp.active.toString)
    )

    formEdit.setFormResponse(vm, styleNone, "Edit Patient Procedure", response, Status.SUCCESS_OK)
  }

  def isEdit(valueMap: ValueMapT): Boolean = {
    Trace.trace(valueMap.contains(patientProcedurePK.label))
    valueMap.contains(patientProcedurePK.label)
  }

  /**
    * Call this when user has clicked create button.  If everything is ok, then create the new user,
    * otherwise show the same screen and communicate the error.
    */
  private def create(valueMap: ValueMapT, response: Response): Unit = {
    val errMap = validateNonEmptyPatientId(valueMap)
    if (errMap.isEmpty) {
      // val existing = PatientProcedure.get(valueMap(patientProcedurePK.label).toLong).get
      val patientProcedure = constructFromParameters(valueMap)
      patientProcedure.insertOrUpdate()
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
