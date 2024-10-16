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

import org.aqa.db.SystemModification
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.sql.Timestamp
import java.text.SimpleDateFormat
import scala.xml.Elem
import scala.xml.XML

object SystemModificationUpdate {
  val systemModificationPKTag = "systemModificationPK"

  val dateFormat = (new WebInputDateTimePicker("Date", 6, 0)).dateFormat

  val latestTag = "latest"
}

class SystemModificationUpdate extends Restlet with SubUrlAdmin {

  private val pageTitleCreate = "Create System Modification"

  private val pageTitleEdit = "Edit System Modification"

  private val date = new WebInputDateTimePicker("Date", 6, 0)

  private val summary = new WebInputText("Summary", 6, 0, "")

  private val description = new WebInputTextArea("Description", 6, 0, "")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val createButton = makeButton("Create", true, ButtonType.BtnPrimary)
  private val saveButton = makeButton("Save", true, ButtonType.BtnPrimary)
  private val deleteButton = makeButton("Delete", false, ButtonType.BtnDanger)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val systemModificationPK = new WebInputHidden(SystemModificationUpdate.systemModificationPKTag)

  private val formCreate = new WebForm(pathOf, List(List(date), List(summary), List(description), List(createButton, cancelButton)))

  private val formEdit = new WebForm(pathOf, List(List(date), List(summary), List(description), List(saveButton, cancelButton, deleteButton, systemModificationPK)))

  /** Return an error if the manufacturer field is not valid. */
  private def validateSummary(valueMap: ValueMapT): StyleMapT = {
    0 match {
      case _ if (valueMap.get(summary.label).get.trim.isEmpty) => Error.make(summary, "Summary can not be empty")
      case _                                                   => styleNone
    }
  }

  private def okToSaveEdited(valueMap: ValueMapT): StyleMapT = {
    validateSummary(valueMap)
  }

  /**
    * Save changes made to form editing an existing machine type.
    */
  private def saveEdits(valueMap: ValueMapT, pageTitle: String, response: Response): Unit = {
    val styleMap = validateSummary(valueMap)
    if (styleMap.isEmpty) {
      (createSystemModificationFromParameters(valueMap)).insertOrUpdate
      SystemModificationList.redirect(response)
    } else formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  /**
    * Create a new machineType
    */
  private def createSystemModificationFromParameters(valueMap: ValueMapT): SystemModification = {
    val pk = {
      val v = valueMap.get(systemModificationPK.label)
      if (v.isDefined)
        Some(v.get.toLong)
      else
        None
    }

    val sysMod =
      new SystemModification(pk, new Timestamp(date.dateFormat.parse(valueMap(date.label)).getTime), getUser(valueMap).get.userPK.get, valueMap(summary.label).trim, valueMap(description.label))
    sysMod
  }

  private def emptyForm(response: Response) = {
    formCreate.setFormResponse(emptyValueMap, styleNone, pageTitleCreate, response, Status.SUCCESS_OK)
  }

  private def create(valueMap: ValueMapT, response: Response) = {
    val styleMap = validateSummary(valueMap)
    if (styleMap.isEmpty) {
      val systemModification = createSystemModificationFromParameters(valueMap)
      logger.info("Creating System Modification: " + systemModification)
      systemModification.insert
      SystemModificationList.redirect(response)
    } else
      formEdit.setFormResponse(valueMap, styleMap, pageTitleEdit, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  /**
    * Populate the fields with the currently row.
    */
  private def edit(systemModification: SystemModification, response: Response) = {
    val valueMap: ValueMapT = Map(
      (systemModificationPK.label, systemModification.systemModificationPK.get.toString),
      (date.label, date.dateFormat.format(systemModification.date)),
      (summary.label, systemModification.summary),
      (description.label, systemModification.description)
    )

    formEdit.setFormResponse(valueMap, styleNone, pageTitleEdit, response, Status.SUCCESS_OK)
  }

  private def getReference(valueMap: ValueMapT): Option[SystemModification] = {
    try {
      val systemModification = SystemModification.get(valueMap(systemModificationPK.label).toLong)
      systemModification
    } catch {
      case _: Throwable => None
    }
  }

  private def isDelete(valueMap: ValueMapT, response: Response): Boolean = {
    if (buttonIs(valueMap, deleteButton)) {
      val value = valueMap.get(systemModificationPK.label)
      if (value.isDefined) {
        SystemModification.delete(value.get.toLong)
        SystemModificationList.redirect(response)
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
    * Determine if the incoming request is to edit an existing machineType.
    */
  private def isEdit(valueMap: ValueMapT, response: Response): Boolean = {
    val inst = getReference(valueMap)
    val value = valueMap.get(systemModificationPK.label)
    if (inst.isDefined) {
      edit(inst.get, response)
      true
    } else
      false
  }

  private def forbidden(response: Response) = {
    val content = {
      <div>
        Only whitelisted users may create or modify System Modification records.
      </div>
    }
    simpleWebPage(content, Status.CLIENT_ERROR_FORBIDDEN, "Modification of System Modification Forbidden", response)
  }

  private def sysModToHtml(sysModPK: Long, response: Response): Elem = {

    val sysMod = SystemModification.get(sysModPK).get

    val descriptionAsHtml = {
      try {
        val elem = XML.loadString(sysMod.description)
        elem
      } catch {
        case t: Throwable => {
          val lines = sysMod.description.split("\n").toSeq.map(s => <p>{s}</p>)
          <div>{lines}</div>
        }
      }
    }

    val content = {
      <div>
        <div class="row">
          <div class="col-md-3">
            <b>Date: </b>{date.dateFormat.format(sysMod.date)}
          </div>
          <div class="col-md-6">
            <b>Summary: </b>{sysMod.summary}
          </div>
        </div>
        <div class="row">
          <div class="col-md-2" style="margin-top:30px">
            <b>Description: </b>
          </div>
        </div>
        <div class="row">
          <div class="col-md-6 col-md-offset-1">
            <br></br>{descriptionAsHtml}
          </div>
        </div>
        <div class="row" style="margin-top:30px">
          <div class="col-md-2">
            <a href={SystemModificationList.path}>Back to list</a>
          </div>
        </div>
      </div>
    }

    content
  }

  private def readOnly(valueMap: ValueMapT, response: Response) = {
    valueMap.get(systemModificationPK.label) match {
      case Some(pkText) => simpleWebPage(sysModToHtml(pkText.toLong, response), Status.SUCCESS_OK, "System Modification", response)
      case _            => forbidden(response)
    }
  }

  private def showLatest(response: Response) = {
    val dateFormat = new SimpleDateFormat("EEE dd MMM yyyy")

    val size = SystemModification.getSize

    val elem = if (size == 0) {
      <a href="/admin/SystemModificationList">System Modifications</a>
    } else {
      val dateText = dateFormat.format(SystemModification.getLatest.get.date)
      val withMods = {
        <a href="/admin/SystemModificationList">
          System Modifications
          <span class="badge badge-secondary" title="Number of system modifications">{size.toString}</span>
          <span title="Date of latest system modification">{dateText}</span>
        </a>
      }
      withMods
    }
    val text = xmlToText(elem)
    setResponse(text, response, Status.SUCCESS_OK)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {
      0 match {
        case _ if valueMap.get(SystemModificationUpdate.latestTag).isDefined => showLatest(response)
        case _ if !userIsWhitelisted(request)                                => readOnly(valueMap, response)
        case _ if buttonIs(valueMap, cancelButton)                           => SystemModificationList.redirect(response)
        case _ if buttonIs(valueMap, createButton)                           => create(valueMap, response)
        case _ if buttonIs(valueMap, saveButton)                             => saveEdits(valueMap, pageTitleEdit, response)
        case _ if isDelete(valueMap, response)                               => Nil
        case _ if isEdit(valueMap, response)                                 => Nil
        case _                                                               => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }
  }
}
