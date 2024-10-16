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

import org.aqa.db.Input
import org.aqa.web.WebUtil._
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.db.User
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.Restlet

import java.io.File
import scala.xml.Elem

object ViewInput {
  private val path = new String((new InstitutionList).pathOf)

  val inputPKTag = "inputPK"
  val summaryTag = "summary"

  private def shouldShowSummary(inputFileExists: Boolean, procedureIsRunning: Boolean, summaryRequested: Boolean, clientOnPendingList: Boolean): Boolean = {
    (inputFileExists, procedureIsRunning, summaryRequested, clientOnPendingList) match {
      case (false, _, _, _)       => true
      case (true, _, true, false) => true
      case _                      => false
    }
  }

}

/**
  * Monitor a process that is running a procedure.  If there is no 'input.*' file, then
  * show some metadata and the directory contents, updating it periodically.  If an
  * 'input.*' is created, then show that instead.
  */
class ViewInput extends Restlet with SubUrlView {

  private def pageTitle = "Input"

  private def inputDir(input: Input): Option[File] = {
    val dirName = input.directory
    if (dirName.isDefined) {
      val dir = new File(dirName.get)
      if (dir.isDirectory) Some(dir) else None
    } else None
  }

  private def dataTime(input: Input): String = {
    val time = input.dataDate
    if (time.isDefined) Util.timeHumanFriendly(input.uploadDate) else "Not Available"
  }

  def summary(input: Input, response: Response): Unit = {
    val dir = inputDir(input)
    val user = User.get(input.userPK.get)
    val machine = Machine.get(input.machinePK.get)

    val machineDescription: Elem = {
      if (machine.isDefined) { <a href={SubUrl.url(SubUrl.admin, "MachineUpdate") + "?machinePK=" + machine.get.machinePK}>{machine.get.id}</a> }
      else { <div>"Not Available"</div> }
    }

    val offset = Config.DataDir.getAbsolutePath.size + 2 + WebServer.tmpDirBaseUrl.size

    /*
        def fileToRow(file: File): Elem = {
            val row = {
                <div class="row">
                    <div class="col-md-2">
                        <a href={ Input.urlOfFile(file.getAbsolutePath) }>{ file.getName }</a>
                    </div>
                </div>
            }
            row
        }

        val content = {
            val status = org.aqa.run.ProcedureStatus.stringToProcedureStatus(input.status)

            val statusElem = {
                if (status.isDefined)
                    <div class="col-md-1" title={ org.aqa.run.ProcedureStatus.descriptionOf(status.get) }>Status: { input.status }</div>
                else
                    <div class="col-md-1">Status: { input.status }</div>
            }

            <div class="row col-md-10 col-md-offset-1">
                <p id="demo">demo demo</p>
                { reload }
                <div class="row">
                    <div class="col-md-3">Machine: { machineDescription }</div>
                    <div class="col-md-2">User: { if (user.isDefined) user.get.fullName else "none" }</div>
                    { statusElem }
                </div>
                <div class="row">
                    <div class="col-md-2 col-md-offset-3">Data Time: { dataTime(input) }</div>
                    <div class="col-md-2">Uploaded: { Util.timeHumanFriendly(input.uploadDate) }</div>
                </div>
                { if (dir.isDefined) dir.get.listFiles.map(f => fileToRow(f)) }
            </div>
        }

        respond(content, "Current Input", response)
     */
  }

  def noInput(response: Response): Unit = {
    response.setEntity("Unknown test input", MediaType.TEXT_PLAIN) // TODO
  }

  private def redirectToDir(dir: Option[File], response: Response) = {}

  override def handle(request: Request, response: Response) = {
    super.handle(request, response)
    try {
      val valueMap = getValueMap(request)

      val showSummary = valueMap.get(ViewInput.summaryTag).isDefined

      val input: Option[Input] = {
        val inputPKText = valueMap.get(ViewInput.inputPKTag)
        if (inputPKText.isDefined) Input.get(inputPKText.get.toLong)
        else None
      }

      0 match {
        case _ if (input.isDefined && showSummary) => summary(input.get, response)
        case _ if (input.isDefined)                => redirectToDir(inputDir(input.get), response)
        case _                                     => noInput(response)
      }
    } catch {
      case e: Exception => internalFailure(response, "Unexpected error: " + e.getMessage)
    }

  }
}
