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
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.aqa.Config
import org.aqa.Crypto
import org.aqa.Util
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.Restlet

import java.io.File
import java.sql.Timestamp
import scala.collection.mutable.HashMap
import scala.xml.Elem

/**
  * Version of Output that only has immutable fields.  Useful for caching.
  */
private class ImmutableOutput(val outputPK: Long, val inputPK: Long, val dir: File, val procedurePK: Long, val userPK: Option[Long], val startDate: Timestamp) {

  def this(output: Output) = this(output.outputPK.get, output.inputPK, WebServer.fileOfResultsPath(output.directory), output.procedurePK, output.userPK, output.startDate)
}

object ViewOutput {
  val path = WebUtil.pathOf(WebUtil.SubUrl.view, ViewOutput.getClass.getName)

  val outputPKTag = "outputPK"
  val summaryTag = "summary"
  val checksumTag = "checksum"

  def viewOutputUrl(outputPK: Long) = {
    val url = ViewOutput.path + "?" + outputPKTag + "=" + outputPK
    url
  }

  private def shouldShowSummary(outputFileExists: Boolean, procedureIsRunning: Boolean, summaryRequested: Boolean, clientOnPendingList: Boolean): Boolean = {
    (outputFileExists, procedureIsRunning, summaryRequested, clientOnPendingList) match {
      case (false, _, _, _)       => true
      case (true, _, true, false) => true
      case _                      => false
    }
  }

  private def shouldRemoveFromPending(outputFileExists: Boolean, procedureIsRunning: Boolean, summaryRequested: Boolean, clientOnPendingList: Boolean): Boolean = {
    (outputFileExists, procedureIsRunning, summaryRequested, clientOnPendingList) match {
      case (false, true, true, true) => false
      case (_, _, _, true)           => true
      case _                         => false
    }
  }

  def showSummary(outputPK: Long, response: Response): Unit = {
    val output = Output.get(outputPK).get
    val procedure = Procedure.get(output.procedurePK).get
    val user = output.getUser
    val elapsed = Util.elapsedTimeHumanFriendly(output.elapsedTime)

    val offset = Config.DataDir.getAbsolutePath.size + 2 + WebServer.tmpDirBaseUrl.size

    def fileToRow(file: File): Elem = {
      val row = {
        <div class="row">
          <div class="col-md-2">
            <a href={WebServer.urlOfResultsFile(file)}>{file.getName}</a>
          </div>
        </div>
      }
      row
    }

    val reload: List[Elem] = {
      if (output.status.equalsIgnoreCase(org.aqa.run.ProcedureStatus.running.toString)) {
        val text = "reloadOn(" + dblQuote(output.outputPK.get.toString) + ", " + dblQuote(secureHashOfOutput(output.outputPK.get)) + ");\n"
        val elem = {
          <script>
            {text}
          </script>
        }
        List(elem)
      } else List()
    }

    val content: Elem = {
      val status = org.aqa.run.ProcedureStatus.stringToProcedureStatus(output.status)

      val statusElem = {
        if (status.isDefined)
          <div class="col-md-1" title={org.aqa.run.ProcedureStatus.descriptionOf(status.get)}>Status: {output.status}</div>
        else
          <div class="col-md-1">Status: {output.status}</div>
      }

      val inputDir = WebServer.urlOfResultsPath(Input.get(output.inputPK).get.directory.get)

      val html: Elem = {
        <div class="row col-md-10 col-md-offset-1">
          <p id="demo">demo demo</p>
          {reload}
          <div class="row">
            <div class="col-md-4">Procedure: {procedure.fullName}</div>
            <div class="col-md-2">User: {if (user.isDefined) wrapAlias(user.get.id) else "none"}</div>
            {statusElem}
          </div>
          <div class="row">
            <div class="col-md-4">
              <a href={inputDir}> {"Input"} </a>
            </div>
            <div class="col-md-2">Started: {Util.timeHumanFriendly(output.startDate)}</div>
            <div class="col-md-2">Elapsed: {elapsed}</div>
          </div>
          { val x = getCachedOutput(output.outputPK.get).dir }
          {getCachedOutput(output.outputPK.get).dir.listFiles.map(f => fileToRow(f))}
        </div>
      }

      html
    }

    respond(content, "Current Output", response)
  }

  def noOutput(response: Response): Unit = {
    val text = "No such output.  Most likely it has been deleted or redone.  Click 'Results' for the latest list of outputs."
    response.setEntity(text, MediaType.TEXT_PLAIN) // TODO
    val content = {
      <div>{text}</div>
    }
    simpleWebPage(content, Status.CLIENT_ERROR_BAD_REQUEST, "No such output", response)
  }

  /** Calculate a secure hash of the time stamps of the child files of a directory. */
  private def secureHashOfDirTime(dir: File): String = Crypto.secureHash(dir.listFiles.foldLeft("")((t, f) => t + f.lastModified.toString))

  /** Cache of output directories.  Once established, they are immutable, so caching works. */
  private val outputDirCache = HashMap[Long, ImmutableOutput]();

  /**
    * Get the cached copy of a an output directory to save excessive database calls.
    */
  private def getCachedOutput(outputPK: Long): ImmutableOutput = {
    outputDirCache.synchronized({
      if (!(outputDirCache.get(outputPK).isDefined)) outputDirCache.put(outputPK, new ImmutableOutput(Output.get(outputPK).get))
      outputDirCache.get(outputPK).get
    })
  }

  private def secureHashOfOutput(outputPK: Long): String = secureHashOfDirTime(getCachedOutput(outputPK).dir)

  /** Determine the last change time of the data. */
  def giveStatus(outputPK: Long, status: String, response: Response) = {
    val secHash = secureHashOfOutput(outputPK)
    response.setStatus(Status.SUCCESS_OK)
    response.setEntity(secHash, MediaType.TEXT_PLAIN)
  }

  /**
    * Redirect the user to the progress of running the procedure.
    *
    * @param response Respond to this HTTP entity.
    *
    * @param isAuto If true, the client wants to be treated like an automatic process (non-human process as
    *   opposed to human using a web browser).  Upon completion, just send the client the return status as
    *   opposed to being redirected to a page containing the results or a progress page.
    *
    * @param outputPK Output being produced.
    */
  def redirectToViewRunProgress(response: Response, isAuto: Boolean, outputPK: Long): Unit = {
    if (isAuto) {
      // set the response to indicate that processing has been successfully started
      response.setStatus(Status.SUCCESS_OK)
      response.setEntity("", MediaType.TEXT_PLAIN)
    } else {
      val suffix = "?" + ViewOutput.outputPKTag + "=" + outputPK
      response.redirectSeeOther(ViewOutput.path + suffix)
    }
  }

  /**
    * Redirect the user to the progress of running the procedure.
    */
  def redirectToViewRunProgress(response: Response, valueMap: ValueMapT, outputPK: Long): Unit = redirectToViewRunProgress(response, isAutoUpload(valueMap), outputPK)

}

/**
  * Monitor a process that is running a procedure.  If there is no 'output.*' file, then
  * show some metadata and the directory contents, updating it periodically.  If an
  * 'output.*' is created, then show that instead.
  */
class ViewOutput extends Restlet with SubUrlView {

  private def pageTitle = "Output"

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = ViewOutput.path + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  private val abortButton = makeButton("Abort", true, ButtonType.BtnPrimary)

  /**
    * Abort the procedure.
    */
  private def abort(valueMap: ValueMapT, request: Request, response: Response) = {
    // TODO
  }

  private def setResponseWithOutputFile(file: File, response: Response) = {
    response.setStatus(Status.SUCCESS_OK)
    response.redirectSeeOther(WebServer.urlOfResultsFile(file))
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    try {
      val valueMap = getValueMap(request)

      val checksum = valueMap.get(ViewOutput.checksumTag)

      val showSummary = valueMap.get(ViewOutput.summaryTag).isDefined

      val output: Option[ImmutableOutput] = {
        try {
          val outputPK = valueMap(ViewOutput.outputPKTag).toLong
          if (Output.get(outputPK).isDefined)
            Some(ViewOutput.getCachedOutput(outputPK))
          else
            None
        }
        catch {
          case _: Throwable => None
        }
      }

      if (output.isDefined) {
        Output.ensureInputAndOutputFilesExist(Output.get(output.get.outputPK).get)
      }

      def displayFile: Option[File] = {
        if (output.isDefined) {
          val display = new File(output.get.dir, Output.displayFilePrefix + ".html")
          if (display.canRead) Some(display) else None
        } else None
      }

      0 match {
        case _ if (output.isDefined && checksum.isDefined) => ViewOutput.giveStatus(output.get.outputPK, checksum.get, response)
        case _ if (output.isDefined && showSummary)        => ViewOutput.showSummary(output.get.outputPK, response)
        case _ if displayFile.isDefined                    => setResponseWithOutputFile(displayFile.get, response)
        case _ if (output.isDefined)                       => ViewOutput.showSummary(output.get.outputPK, response)
        case _                                             => ViewOutput.noOutput(response)
      }
    } catch {
      case e: Exception => internalFailure(response, "Unexpected error: " + e.getMessage)
    }

  }
}
