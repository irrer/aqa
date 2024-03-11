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

import edu.umro.ScalaUtil.PrettyXML
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.OutputNote
import org.aqa.db.User
import org.aqa.web.OutputHeading.returnUrlTag
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.text.SimpleDateFormat
import scala.xml.Elem

object OutputHeading {

  val returnUrlTag = "returnUrl"

  private val path = new String((new OutputHeading).pathOf)

  private def pathWithOutput(outputPK: Long): String = s"$path?${ViewOutput.outputPKTag}=$outputPK"

  def reference(outputPK: Long): Elem = <div id="headerOutputPK" headerOutputPK={s"${pathWithOutput(outputPK)}"}></div>
}

class OutputHeading extends Restlet with SubUrlAdmin with Logging {

  private def showOutputHeading(extendedData: ExtendedData, valueMap: ValueMapT, response: Response): Unit = {

    val showMachine = {
      <div class="col-md-1">
        <h2 title="Treatment machine.  Click for details.">{MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, extendedData.machine.id)}</h2>
      </div>
    }

    def wrapElement(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
      val html =
        if (asAlias) {
          <span aqaalias="">{value}</span>
        } else {
          val valueList = value.split("\n");
          { <span>{valueList.head}{valueList.tail.map(line => { <span><br/> {line} </span> })}</span> }
        }

      { <div class={"col-md-" + col}><em>{name}:</em><br/>{html}</div> }

    }
    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")

    def dataAcquisitionDate = {
      if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
      else "unknown"
    }
    val elapsed: String = {
      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _             => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    def noteElem(valueMap: ValueMapT) = {
      val noteText: String = OutputNote.getByOutput(extendedData.outputPK) match {
        case Some(outNote) => outNote.contentAsText
        case _             => ""
      }

      // when done editing, return to this URL
      val url = {
        val editUrl = OutputHeading.pathWithOutput(extendedData.outputPK)
        val returnUrl = if (valueMap.contains(returnUrlTag)) valueMap(returnUrlTag) else ViewOutput.viewOutputUrl(extendedData.outputPK)
        s"$editUrl&$editTag=true&$returnUrlTag=$returnUrl"
      }
      <span>{noteText}<a href={url}> Edit</a></span>
    }

    def content(valueMap: ValueMapT) = {
      <div class="row">
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {showMachine}
            {wrapElement(2, "Institution", extendedData.institution.name, asAlias = true)}
            {wrapElement(1, "Data Acquisition", dataAcquisitionDate, asAlias = false)}
            {wrapElement(1, "Analysis Started", twoLineDate.format(extendedData.output.startDate), asAlias = false)}
            {wrapElement(1, "User", extendedData.user.id, asAlias = true)}
            {wrapElement(1, "Elapsed", elapsed, asAlias = false)}
            {wrapElement(1, "Procedure", procedureDesc, asAlias = false)}
            <div class="col-md-1">{OutputList.redoUrl(extendedData.outputPK)}</div>
          </div>
        </div>
        <div class="row">
          <div class={"col-md-10 col-md-offset-1"}>
            {noteElem(valueMap)}
          </div>
        </div>
      </div>
    }

    val text = PrettyXML.xmlToText(content(valueMap))
    // setResponse(wrapBody(content, "Edit Note"), response, Status.SUCCESS_OK)
    setResponse(text, response, Status.SUCCESS_OK)
  }

  private val editTag = "edit"

  /**
    * No output, so just return empty HTML.
    * @param response Put response here.
    */
  private def emptyResponse(response: Response): Unit = {
    val text = PrettyXML.xmlToText(<span></span>)
    WebUtil.setResponse(text, response, Status.SUCCESS_OK)
  }

  /**
    * Determine if the user is authorized to change the note.  They must either be
    * from the same institution or be whitelisted.
    * @param extendedData Output metadata.
    * @param user web user
    * @return True if authorized to change note.
    */
  private def isAuthorized(extendedData: ExtendedData, user: Option[User]): Boolean = {

    0 match {
      case _ if user.isDefined && WebUtil.userIsWhitelisted(user.get.id_real.get)           => true
      case _ if user.isDefined && user.get.institutionPK == extendedData.user.institutionPK => true
      case _                                                                                => false
    }
  }

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val columns = (name.length / 15) + 1
    new FormButton(name, columns, 0, subUrl, pathOf, buttonType)
  }

  private val saveButton = makeButton("Save", primary = true, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)
  private val buttonList: WebRow = List(cancelButton, saveButton)

  private def noteField() = new WebInputTextArea("Note", 6, 0, "", true)
  private def outputPkField = new WebInputHidden("outputPK")

  private def returnUrlField = new WebInputHidden(returnUrlTag)
  private def fieldList(): WebRow = List(noteField(), outputPkField, returnUrlField)

  /**
    * Redirect back to the original view.
    * @param extendedData Metadata
    * @param valueMap HTML parameters
    * @param response Put response here
    */
  private def redirectBack(extendedData: ExtendedData, valueMap: ValueMapT, response: Response): Unit = {
    valueMap.get(returnUrlTag) match {
      case Some(url) =>
        response.redirectSeeOther(url)
      case _ =>
        showOutputHeading(extendedData, valueMap, response)
    }
  }

  /**
    * Save the result of the edit.  If there was an old note, then update it.  If not, then create a new one.
    * @param extendedData Metadata for output
    * @param valueMap Values from form.
    * @param response HTML response.
    */
  private def saveNote(extendedData: ExtendedData, valueMap: ValueMapT, response: Response): Unit = {
    val content = valueMap(noteField().label).getBytes()

    OutputNote.getByOutput(extendedData.outputPK) match {
      case Some(outputNote) =>
        val outNote = outputNote.copy(content = content)
        outNote.insertOrUpdate()
      case _ =>
        val outNote = OutputNote(None, extendedData.outputPK, "", content)
        outNote.insert
    }

    redirectBack(extendedData, valueMap, response)
  }

  /**
    * Show user a page for editing the note.
    * @param extendedData For this output.
    * @param response Put web response here.
    */
  private def editNote(valueMap: ValueMapT, extendedData: ExtendedData, response: Response): Unit = {

    val outputNoteContent = {
      OutputNote.getByOutput(extendedData.outputPK) match {
        case Some(note) => note.contentAsText
        case _          => ""
      }
    }

    val formEdit = new WebForm(pathOf, List(fieldList(), buttonList))

    val returnFieldTuple = {
      val value = if (valueMap.contains(returnUrlTag)) valueMap(returnUrlTag) else ViewOutput.viewOutputUrl(extendedData.outputPK)
      (returnUrlField.label, value)
    }

    val newValueMap: ValueMapT = valueMap ++ Map((noteField().label, outputNoteContent), returnFieldTuple)

    // val content = formEdit.toHtml(newValueMap, errorMap = styleNone, response = Some(response))
    //setResponse(wrapBody(content, "Edit Output"), response, Status.SUCCESS_OK)
    formEdit.setFormResponse(newValueMap, errorMap = styleNone, "Edit Note", response = response, Status.SUCCESS_OK)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {

      val outputPk: Option[Long] = valueMap.get(ViewOutput.outputPKTag).map(_.toLong)
      val output: Option[Output] = if (outputPk.isDefined) Output.get(outputPk.get) else None
      val extendedData = if (output.isDefined) Some(ExtendedData.get(output.get)) else None
      val edit = valueMap.contains(editTag) && valueMap(editTag).trim.toLowerCase.toBoolean
      val save = valueMap.contains(saveButton.label)
      val cancel = valueMap.contains(cancelButton.label)
      val user = getUser(valueMap)

      0 match {
        case _ if extendedData.isDefined && save && isAuthorized(extendedData.get, user) => saveNote(extendedData.get, valueMap, response)
        case _ if extendedData.isDefined && edit && user.isDefined                       => editNote(valueMap, extendedData.get, response)
        case _ if extendedData.isDefined && cancel && user.isDefined                     => redirectBack(extendedData.get, valueMap, response)
        case _ if extendedData.isDefined                                                 => showOutputHeading(extendedData.get, valueMap, response)
        case _                                                                           => emptyResponse(response)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
