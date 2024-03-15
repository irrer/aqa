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
import org.aqa.Config
import org.aqa.run.ProcedureStatus
import org.aqa.web.OutputHeading.reference
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

    val machineElem = {
      <h2 title="Treatment machine.  Click for details.">{MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, extendedData.machine.id)}</h2>
    }

    val institutionElem = {
      <span aqaalias="">{extendedData.institution.name}</span>
    }

    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")

    def dataAcquisitionDateElem: Elem = {
      val text =
        if (extendedData.output.dataDate.isDefined)
          twoLineDate.format(extendedData.output.dataDate.get)
        else "unknown"
      <span>
        <em>Data Acquisition:</em>
        <br>{text}</br>
      </span>
    }

    def analysisStartDateElem: Elem = {
      <span>
        <em>Data Analysis:</em>
        <br>{twoLineDate.format(extendedData.output.startDate)}</br>
      </span>
    }

    def userElem: Elem = {
      <span>
        <span aqaalias="">{extendedData.user.id}</span>
      </span>
    }

    def elapsedTimeElem: Elem = {

      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _             => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime

      <span>
        <em>Elapsed:</em>
        <br>{Util.elapsedTimeHumanFriendly(elapsed)}</br>
      </span>
    }

    def procedureElem: Elem = {
      <h2>{extendedData.procedure.name}</h2>
    }

    def machineTypeElem: Elem = {
      <span>
        {extendedData.machineType.manufacturer}
        <br>{extendedData.machineType.model}</br>
      </span>
    }

    def epidElem: Elem = {
      <span>
        {extendedData.epid.model}
      </span>
    }

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
      val noteContent: Elem = {
        if (noteText.trim.isEmpty)
          <em style="color: #B0B0B0;">User defined note.  Click 'Note' to add.</em>
        else
          <span>{noteText}</span>
      }
      <span><a title="Note regarding this output.  Click here to edit." href={url}>Note: </a>{noteContent}</span>
    }

    val passed = {
      extendedData.output.status.equals(ProcedureStatus.pass.toString) || extendedData.output.status.equals(ProcedureStatus.done.toString)
    }

    val redoElem: Elem = OutputList.redoUrl(extendedData.outputPK)

    val passFailImage = {
      if (passed) {
        <div title="Passed!">
          <img src={Config.passImageUrl} width="128"/>
        </div>
      } else {
        <div title="Failed">
          <img src={Config.failImageUrl} width="128"/>
        </div>
      }
    }

    val row1: Elem = {
      val align = "text-align:center;"
      val padding = "padding-right:15px; padding-left:15px;"
      val wrap = "white-space:nowrap;"
      val border = "border-right: 1px solid #e0e0e0;"
      val noBorder = "border:1px solid white;"
      val trStyle = s"$align $padding $wrap $noBorder $border"
      <table>
        <tr>
          <td style={s"$padding $noBorder"} > {passFailImage} </td>
          <td style={s"$align $padding $noBorder"} > {procedureElem} </td>
          <td style={s"$align $padding $noBorder"} > {machineElem} </td>
          <td style={trStyle + " padding-left:30px;"} > {machineTypeElem} </td>
          <td style={s"$align $padding $border $noBorder"} > {epidElem} </td>
          <td style={trStyle} > {dataAcquisitionDateElem} </td>
          <td style={trStyle} > {analysisStartDateElem} </td>
          <td style={trStyle} > {elapsedTimeElem} </td>
          <td style={trStyle} title="User and institution"> {userElem} <br/> {institutionElem} </td>
          <td style={s"$align $padding $wrap $noBorder"} > {redoElem} </td>
        </tr>
      </table>
    }

    val row2: Elem = {
      reference(extendedData.outputPK)
    }

    def content(valueMap: ValueMapT) = {
      <div class="row">
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {row1}
          </div>
          <div class="col-md-10 col-md-offset-1">
            {row2}
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

    val ok = 0 match {
      case _ if user.isDefined && user.get.getRealId.isDefined && WebUtil.userIsWhitelisted(user.get.getRealId.get) => true
      case _ if user.isDefined && user.get.institutionPK == extendedData.user.institutionPK                         => true
      case _                                                                                                        => false
    }
    ok
  }

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val columns = (name.length / 15) + 1
    new FormButton(name, columns, 0, subUrl, pathOf, buttonType)
  }

  private val saveButton = makeButton("Save", primary = true, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)
  private val buttonList: WebRow = List(cancelButton, saveButton)

  private def noteField() = new WebInputTextArea("Note", 6, 0, "User defined note regarding output ...", true)
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

    val hasContent = new String(content).trim.nonEmpty

    val oldOutputNote = OutputNote.getByOutput(extendedData.outputPK)

    (hasContent, oldOutputNote.isDefined) match {
      // note already exists, and user is putting new content in it. Update the note with new content.
      case (true, true) =>
        val newOutputNote = oldOutputNote.get.copy(content = content)
        newOutputNote.insertOrUpdate()

      // note already exists, but user is deleting it.  Delete the note.
      case (false, true) =>
        OutputNote.delete(oldOutputNote.get.outputNotePK.get)

      // user is putting content in note, but it does not exist.  Make a new note with the user's content.
      case (true, false) =>
        val outNote = OutputNote(None, extendedData.outputPK, "", content)
        outNote.insert

      // user is not putting content in note, and it does not exist. Do nothing.
      case (false, false) =>
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
        case _ if extendedData.isDefined && save && isAuthorized(extendedData.get, user)    => saveNote(extendedData.get, valueMap, response)
        case _ if extendedData.isDefined && save && (!isAuthorized(extendedData.get, user)) => redirectBack(extendedData.get, valueMap, response)
        case _ if extendedData.isDefined && edit && user.isDefined                          => editNote(valueMap, extendedData.get, response)
        case _ if extendedData.isDefined && cancel && user.isDefined                        => redirectBack(extendedData.get, valueMap, response)
        case _ if extendedData.isDefined                                                    => showOutputHeading(extendedData.get, valueMap, response)
        case _                                                                              => emptyResponse(response)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
