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
import org.aqa.db.MachineLog
import org.aqa.db.MaintenanceRecord
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.sql.Timestamp
import java.util.Date
import scala.xml.Elem

object MaintenanceRecordUpdate {
  val maintenanceRecordPKTag = "maintenanceRecordPK"
}

class MaintenanceRecordUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create Maintenance Record"

  private val pageTitleEdit = "Edit Maintenance Record"

  private val maintenanceRecordPK = new WebInputHidden(MaintenanceRecordUpdate.maintenanceRecordPKTag)

  private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private val dateTime = new WebInputDateTimePicker("Date / Time", 6, 0)

  private def getOutputReference(valueMap: ValueMapT): Elem = {
    try {
      val pk = valueMap(MaintenanceRecordUpdate.maintenanceRecordPKTag).toLong
      val outputPK = MaintenanceRecord.get(pk).get.outputPK.get
      val url = "/view/ViewOutput?outputPK=" + outputPK
      <a href={url}>View Originating Output</a>
    } catch {
      case _: Throwable => <div>No related output</div>
    }
  }

  private val outputReference = new WebPlainText("RelatedOutput", false, 6, 0, getOutputReference)

  private val summary = new WebInputText("Summary", 6, 0, "")

  private val category = new WebInputSelect("Category", 6, 0, (_: Option[Response]) => Config.MaintenanceCategoryList.map(m => (m.Name, m.Name)))

  private val description = new WebInputTextArea("Description", 6, 0, "")

  private def machLogHtml(valueMap: ValueMapT): Elem = {
    val mr = {
      val pkText = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag)
      if (pkText.isDefined)
        MaintenanceRecord.get(pkText.get.toLong)
      else
        None
    }
    if (mr.isDefined && mr.get.machineLogPK.isDefined && mr.get.machineLogNodeIndex.isDefined) {
      val machLog = MachineLog.get(mr.get.machineLogPK.get).get
      val text = machLog.content.replace("\n", WebUtil.nl).replace(" ", WebUtil.nbsp)

      // prefix of full XML text starting at the first Node.
      val pre = {
        val i = machLog.content.indexOf("<Environment>")
        val t = if (i < 0) machLog.content else machLog.content.drop(i)
        val p = t.replaceAll(".*<.Environment>", "").take(100) + "..."
        p
      }
      val id = "MachLog"

      <div>
        <b>Originating Machine Log</b>
        <div class="container">
          <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#" + id}>{WebUtil.magnify}</button>
          <span style="white-space: pre-line;">{pre}</span>
          <div id={id} class="collapse">
            <pre>{WebUtil.nl + text}</pre>
          </div>
        </div>
      </div>
    } else
      <span></span>
  }

  private val machineLog = new WebPlainText(" ", showLabel = false, col = 6, offset = 0, machLogHtml)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val createButton = makeButton("Create", primary = true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", primary = true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", primary = false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)

  val fieldList: List[WebRow] = List(List(summary), List(category), List(description), List(outputReference), List(machineLog), List(dateTime))

  val createButtonList: WebRow = List(createButton, cancelButton, machinePK)
  val editButtonList: WebRow = List(saveButton, cancelButton, deleteButton, machinePK, maintenanceRecordPK)

  private val formCreate = new WebForm(pathOf, fieldList :+ createButtonList)

  private val formEdit = new WebForm(pathOf, fieldList :+ editButtonList)

  private def redirectToList(response: Response, valueMap: ValueMapT): Unit = {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MaintenanceRecordList.getClass.getName) + "?" + MachineUpdate.machinePKTag + "=" + valueMap(MachineUpdate.machinePKTag)
    response.redirectSeeOther(path)
  }

  /**
    * Make sure that the user is authorized.  They must be from the same institution as the machine.
    *
    * Allowing a whitelisted user is probably not a good idea because the ue
    */
  private def isAuthorized(request: Request, institutionPK: Long): Boolean = {
    val ok = {
      getUser(request) match {
        case Some(user) => user.institutionPK == institutionPK
        case _          => false
      }
    }
    ok
  }

  private def emptySummary(valueMap: ValueMapT): StyleMapT = {
    val summaryText = valueMap(summary.label).trim
    val isEmpty = summaryText.trim.isEmpty
    if (isEmpty) Error.make(summary, summary.label + " can not be empty")
    else styleNone
  }

  private def validateAuthentication(valueMap: ValueMapT, request: Request): StyleMapT = {

    val machPK = {
      if (valueMap.contains(machinePK.label)) {
        valueMap(machinePK.label).toLong
      } else {
        MaintenanceRecord.get(valueMap(maintenanceRecordPK.label).toLong).get.machinePK
      }
    }
    val instPK = Machine.get(machPK).get.institutionPK

    if (isAuthorized(request, instPK)) styleNone
    else Error.make(summary, "You are not allowed to modify maintenance records for machines from other institutions.  Use Cancel to return to list.")
  }

  /**
    * If this was created from a machine log, then do not allow the user to modify it.
    * @param valueMap Parameter list.
    * @return Message if it was created from a machine log.
    */
  private def fromMachineLog(valueMap: ValueMapT): StyleMapT = {
    val machineLogPK = MaintenanceRecord.get(valueMap(maintenanceRecordPK.label).toLong).get.machineLogPK
    if (machineLogPK.isDefined)
      Error.make(summary, "You are not allowed to modify maintenance records that were created from machine logs.  Use Cancel to return to list.")
    else
      styleNone
  }

  private def checkFields(valueMap: ValueMapT, request: Request): StyleMapT = validateAuthentication(valueMap, request) ++ emptySummary(valueMap)

  /**
    * Create a new maintenanceRecord
    */
  private def createMaintenanceRecordFromParameters(valueMap: ValueMapT, request: Request): MaintenanceRecord = {
    val mrPK: Option[Long] = {
      val e = valueMap.get(MaintenanceRecordUpdate.maintenanceRecordPKTag)
      if (e.isDefined) Some(e.get.toLong) else None
    }

    val machPK = machinePK.getValOrEmpty(valueMap).trim.toLong
    val dt = new Timestamp(dateTime.dateFormat.parse(dateTime.getValOrEmpty(valueMap)).getTime)
    val uPK = getUser(request).get.userPK.get
    val cat = valueMap(category.label)

    new MaintenanceRecord(
      maintenanceRecordPK = mrPK,
      category = cat,
      machinePK = machPK,
      creationTime = dt,
      userPK = uPK,
      outputPK = None, // outputPK
      machineLogPK = None,
      machineLogNodeIndex = None,
      summary.getValOrEmpty(valueMap).trim,
      description.getValOrEmpty(valueMap).trim
    )

  }

  private def emptyForm(response: Response, valueMap: ValueMapT): Unit = {
    val vm = Map((machinePK.label, valueMap(machinePK.label)), (dateTime.label, dateTime.dateFormat.format(new Date)))
    formCreate.setFormResponse(vm, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  /**
    * Save changes made to form.
    */
  private def save(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = checkFields(valueMap, response.getRequest) ++ fromMachineLog(valueMap)
    if (styleMap.isEmpty) {
      createMaintenanceRecordFromParameters(valueMap, response.getRequest).insertOrUpdate()
      redirectToList(response, valueMap)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  private def create(valueMap: ValueMapT, response: Response): Unit = {
    val styleMap = checkFields(valueMap, response.getRequest)
    if (styleMap.isEmpty) {
      createMaintenanceRecordFromParameters(valueMap, response.getRequest).insert
      redirectToList(response, valueMap)
    } else formCreate.setFormResponse(valueMap, styleMap, pageTitleCreate, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  private def edit(inst: MaintenanceRecord, response: Response): Unit = {
    val valueMap = Map(
      (maintenanceRecordPK.label, inst.maintenanceRecordPK.get.toString),
      (category.label, inst.category),
      (machinePK.label, inst.machinePK.toString),
      (dateTime.label, dateTime.dateFormat.format(inst.creationTime)),
      (summary.label, inst.summary),
      (description.label, inst.description)
    )
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
    value.isDefined && value.get.equals(button.label)
  }

  /**
    * Determine if the incoming request is to edit an existing MaintenanceRecord.
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
        case _ if buttonIs(valueMap, cancelButton) => redirectToList(response, valueMap)
        case _ if buttonIs(valueMap, createButton) => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton)   => save(valueMap, response)
        case _ if isDelete(valueMap, response)     =>
        case _ if isEdit(valueMap, response)       =>
        case _                                     => emptyForm(response, valueMap)
      }
    } catch {
      case t: Throwable => WebUtil.internalFailure(response, t)
    }
  }
}
