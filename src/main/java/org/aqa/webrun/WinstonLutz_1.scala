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

package org.aqa.webrun

import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.run.Run
import org.aqa.web.WebUtil._
import org.aqa.Util
import org.aqa.web.WebUtil
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status

import java.io.File

object WinstonLutz_1 {
  val parametersFileName = "parameters.xml"
  val WinstonLutz_1PKTag = "WinstonLutz_1PK"
}

class WinstonLutz_1(procedure: Procedure) extends WebRunProcedure(procedure) {

  /** Maximum tongue and groove offset in mm.  Exceeding this value probably indicates a user error. */
  private val maxTongueAndGrooveOffset = 10.0

  private val pageTitle = "Winston Lutz"

  def machineList(response: Option[Response]) = Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

  private val machine = new WebInputSelect("Machine", 6, 0, machineList)

  private val tongueAndGrooveX = new WebInputText("Tongue and Groove correction X", 3, 0, "In mm. Use 0 if not known")

  private val tongueAndGrooveY = new WebInputText("Tongue and Groove correction Y", 3, 0, "In mm. Use 0 if not known")

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private val form = new WebForm(procedure.webUrl, List(List(machine), List(tongueAndGrooveX, tongueAndGrooveY), List(runButton, cancelButton)), 6)

  private val defaultValueMap: ValueMapT =
    Map((tongueAndGrooveX.label, "0.0"), (tongueAndGrooveY.label, "0.0"))

  private def emptyForm(response: Response) = {
    form.setFormResponse(defaultValueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

  private def validateTongueAndGroove(valueMap: ValueMapT, input: IsInput): StyleMapT = {
    val err = Error.make(input, "Must be a valid floating point number from -" + maxTongueAndGrooveOffset + " to " + maxTongueAndGrooveOffset)
    try {
      val tg = input.getValOrEmpty(valueMap).toFloat
      if ((tg >= (-maxTongueAndGrooveOffset)) && (tg <= (maxTongueAndGrooveOffset))) styleNone
      else err
    } catch {
      case e: Exception => err
    }
  }

  private def validateFiles(valueMap: ValueMapT): StyleMapT = {
    sessionDir(valueMap) match {
      case Some(dir) if (!dir.isDirectory)  => Error.make(form.uploadFileInput.get, "No files have been uploaded (no directory)") // TODO
      case Some(dir) if (dir.list.size < 2) => Error.make(form.uploadFileInput.get, "At least two files are required.") // TODO
      case _                                => styleNone
    }
  }

  private def validate(valueMap: ValueMapT): StyleMapT = {
    validateFiles(valueMap) ++
      validateTongueAndGroove(valueMap, tongueAndGrooveX) ++
      validateTongueAndGroove(valueMap, tongueAndGrooveY)
  }

  private def makeParameterFile(valueMap: ValueMapT): Unit = {

    val xml = {
      <WinstonLutzParameters>
        <TongueAndGrooveOffsetX>{tongueAndGrooveX.getValOrEmpty(valueMap)}</TongueAndGrooveOffsetX>
        <TongueAndGrooveOffsetY>{tongueAndGrooveY.getValOrEmpty(valueMap)}</TongueAndGrooveOffsetY>
      </WinstonLutzParameters>
    }

    sessionDir(valueMap) match {
      case Some(dir) => {
        val paramFile = new File(dir, WinstonLutz_1.parametersFileName)
        Util.writeFile(paramFile, WebUtil.xmlToText(xml))
      }
      case _ => throw new RuntimeException("WinstonLutz_1.makeParameterFile : session dir is not valid")
    }
  }

  /**
    * Run the procedure.
    */
  private def run(valueMap: ValueMapT, request: Request, response: Response) = {
    val errMap = validate(valueMap)
    if (errMap.isEmpty) {
      makeParameterFile(valueMap)
      val machinePK = machine.getValOrEmpty(valueMap).toLong
      sessionDir(valueMap) match {
        case Some(dir) => {
          val dtp = Util.dateTimeAndPatientIdFromDicom(dir)
          Run.run(procedure, Machine.get(machinePK).get, dir, request, response, dtp.PatientID, dtp.dateTime, None)
        }
        case _ => throw new RuntimeException("WinstonLutz_1.run : session dir is not valid")
      }
    } else
      form.setFormResponse(valueMap, errMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = defaultValueMap ++ getValueMap(request)

    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
        case _ if buttonIs(valueMap, runButton)    => run(valueMap, request, response)
        case _                                     => emptyForm(response)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
