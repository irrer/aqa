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

import org.aqa.Util
import org.aqa.db.CachedUser
import org.aqa.db.DataValidity
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.OutputList.deleteTag
import org.aqa.web.OutputList.outputPKTag
import org.aqa.web.OutputList.statusTag
import org.aqa.web.WebUtil._
import org.aqa.webrun.WebRun
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.routing.Filter

import java.text.SimpleDateFormat
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Elem

/**
  * List the outputs to let users re-visit results.
  */

object OutputList {
  private val deleteTag = "delete"
  val redoTag = "redo"
  private val confirmTag = "confirm"
  private val statusTag = "status"
  private val outputPKTag = "outputPK"

  val path = new String((new OutputList).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)

  def redoUrl(outputPK: Long): Elem = {
    <a title="Click to re-run analysis. New results will replace previous results." href={path + "?" + redoTag + "=" + outputPK}>Redo</a>
  }

  /** Flag that controls whether a bulk redo is running.   Set to false to suspend the bulk redo. */
  private val bulkRedoIsRunning = new AtomicBoolean(true)
}

class OutputList extends GenericList[Output.ExtendedValues] with WebUtil.SubUrlView {

  override def listName = "Output"

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show output from all institutions, then click 'Refresh'"), 2, 0)
  val refresh: FormButton = makeButton("Refresh", ButtonType.BtnPrimary)

  /**
    * Generate the contents for the the "To Do" list.  It consists of
    * any outputs that exist.
    * @param valueMap parameter list.
    * @return List of output PKs
    */
  private def getToDoHtml(valueMap: ValueMapT): Elem = {
    val requestSeq = getRequestedSeq(valueMap)
    val todoText =
      if (requestSeq.isEmpty)
        ""
      else {
        val response: Response = null // this never gets used
        val todoSet = getData(valueMap, response).map(e => e.output_outputPK)
        requestSeq.filter(pk => todoSet.contains(pk)).mkString(" ")
      }
    <div title="List of outputPK's need to be done">{todoText}</div>
  }

  //noinspection ScalaUnusedSymbol
  private def getRunningHtml(valueMap: ValueMapT): Elem = {
    <div>{OutputList.bulkRedoIsRunning.toString}</div>
  }

  /**
    * Generate the contents for the the "Done" list.  It consists of
    * any outputs that do not exist.
    * @param valueMap parameter list.
    * @return List of output PKs
    */
  private def getDoneHtml(valueMap: ValueMapT): Elem = {
    val requestSeq = getRequestedSeq(valueMap)
    val response: Response = null // this never gets used
    val existing = getData(valueMap, response).map(e => e.output_outputPK).toSet
    // val donePkSet = requestSet.diff(existing)
    val doneText = requestSeq.filterNot(pk => existing.contains(pk)).mkString(" ")
    <div title="List of outputPK's that no longer exist or never existed">{doneText}</div>
  }

  private def bulkRedoInstructions: WebPlainText = {
    val title =
      "To perform a bulk redo, enter a list of outputPKs in the 'Request List' box." + titleNewline +
        "Clicking the Refresh button will refresh the list containing just the 'Request List' entries." + titleNewline +
        "outputPKs may be separated by any whitespace, comma, or non-digit." + titleNewline +
        "If the 'Request List' list is empty then all outputs will be listed." + titleNewline +
        "Each output redo is performed sequentially so as not to overload the server." + titleNewline +
        "The page will refresh when all of the outputs have been performed." + titleNewline +
        "Once started, the only way to stop processing the list is to restart the server." + titleNewline +
        "You may monitor progress by copying and pasting list to another results" + titleNewline +
        "page and refreshing that screen." + titleNewline

    def elem(valueMap: ValueMapT) = {
      val quantities: Elem = {
        val requestList = getRequestedSeq(valueMap)
        if (requestList.isEmpty)
          <div></div>
        else {
          val response: Response = null // this never gets used
          val todoSet = getData(valueMap, response).map(e => e.output_outputPK)
          <div class="row">
            <div class="col-md-2">Requested: {requestList.size}</div>
            <div class="col-md-2"><span style="white-space: nowrap;">To Do:</span>{todoSet.size}</div>
            <div class="col-md-2">Done: {(requestList.size - todoSet.size).toString}</div>
          </div>
        }
      }
      <div title={title}>Hover here for bulk redo instructions.  {quantities}</div>
    }
    new WebPlainText("Bulk Redo Instructions", false, 3, 0, (valueMap: ValueMapT) => elem(valueMap))
  }

  private val requestList = new WebInputTextArea("Request List", 4, 0, "List of Output public keys to Redo")
  private val todoList = new WebPlainText("To Do", true, 4, 0, getToDoHtml)
  val doneList = new WebPlainText("Done", true, 4, 0, getDoneHtml)
  private val redoAll: FormButton = makeButton("Redo All", ButtonType.BtnDefault)
  private val stopButton: FormButton = makeButton("Stop", ButtonType.BtnDefault)
  private val resumeButton: FormButton = makeButton("Resume", ButtonType.BtnDefault)
  private val runningState = new WebPlainText("Running", true, 1, 0, getRunningHtml)

  override def htmlFieldList(valueMap: ValueMapT): List[WebRow] = {
    val webRow = List(checkbox, refresh)
    def instructRow = List(bulkRedoInstructions, redoAll, stopButton, resumeButton, runningState)
    def redoRow = List(requestList, todoList, doneList)

    if (userIsWhitelisted(valueMap))
      List(webRow, instructRow, redoRow)
    else
      List(webRow)
  }

  private val startTimeFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss")

  //noinspection SameParameterValue
  private def compareByStartTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Boolean = a.output_startDate.compareTo(b.output_startDate) > 0

  private def inputTime(extendedValues: Output.ExtendedValues): String = {
    val date = extendedValues.input_dataDate
    if (date.isDefined) startTimeFormat.format(date.get) else "unknown date"
  }

  private def getUrl(outputPK: Long): String = {
    val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + outputPK
    url
  }

  //noinspection SameParameterValue
  private def startTimeUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Data analysis time" href={getUrl(extendedValues.output_outputPK)}> {startTimeFormat.format(extendedValues.output_startDate)}</a>
  }

  private val validityList = DataValidity.values.toSeq.map(_.toString)

  private def setState(extendedValues: Output.ExtendedValues): Elem = {

    val outputPK = extendedValues.output_outputPK
    val id = "DataValidity" + outputPK

    val current = extendedValues.dataValidity.toString

    /*
    val deleteSelection = {
      <option value={(DataValidity.values.size + 1).toString}>
        <a title="Click to delete.  Can NOT be undone" href={OutputList.path + "?" + OutputList.deleteTag + "=" + outputPK}>Delete</a>
      </option>
    }
     */

    val choiceList = {
      def opt(dv: DataValidity.Value): Elem = {
        val dvs = dv.toString
        if (dv.toString.equals(current)) {
          <option selected="selected" value={dvs}>{dvs}</option>
        } else {
          <option value={dvs}>{dvs}</option>
        }
      }

      val list = DataValidity.values.map(opt) + { <option title="be undone" value={deleteTag}>delete</option> }
      list
    }

    val html = {
      <select value={current} name={id} id={id} class="form-control" onchange={s"StatusChange($id.value, $outputPK)"}>
        {choiceList}
      </select>
    }
    html
  }

  private def redoUrl(extendedValues: Output.ExtendedValues): Elem = {
    OutputList.redoUrl(extendedValues.output_outputPK)
  }

  private type ColT = Output.ExtendedValues // Column Type

  private val institutionCol = new Column[ColT]("Institution", _.institution_name, colT => wrapAlias(colT.institution_name))

  private val userCol = new Column[ColT]("User", _.user_id, colT => wrapAlias(colT.user_id))

  //noinspection ConvertibleToMethodValue
  private val startTimeCol = new Column[ColT]("Analysis", compareByStartTime _, startTimeUrl _)

  private val inputFileCol = new Column[ColT]("Acquisition", inputTime)

  private val redoCol = new Column[ColT]("Redo", _ => "Redo", redoUrl)

  private val deleteCol = new Column[ColT]("Delete/State", _.dataValidity.toString, setState)

  private val procedureCol = new Column[ColT]("Procedure", d => d.procedure_name + " " + d.procedure_version)

  private val machineCol = new Column[ColT]("Machine", _.machine_id, colT => wrapAlias(colT.machine_id))

  override val columnList: Seq[Column[ColT]] = Seq(startTimeCol, inputFileCol, redoCol, procedureCol, machineCol, institutionCol, userCol, deleteCol)

  private val entriesPerPage = 600 // should support pagination

  /**
    * Get the set of outputs that user wants to redo.  Return empty set if none.  This
    * feature is only available to whitelisted users.
    */
  private def getRequestedSeq(valueMap: ValueMapT): Seq[Long] = {
    val isWhitelisted = userIsWhitelisted(valueMap)
    val outPkSeq =
      if (isWhitelisted && valueMap.contains(requestList.label) && valueMap(requestList.label).trim.nonEmpty) {
        // get all longs
        valueMap(requestList.label).replaceAll("[^0-9]", " ").split(" ").toSeq.filter(t => t.nonEmpty).map(t => t.toLong)
      } else
        Seq()
    outPkSeq
  }

  /**
    * Get the extended data for the given list of outputs.
    *
    * @param valueMap Parameter list.
    * @param response Put data here.  Not used for this implementation of the superclass.
    * @return List of outputs with associated values.
    */
  override def getData(valueMap: ValueMapT, response: Response): Seq[Output.ExtendedValues] = {

    val isWhitelisted = userIsWhitelisted(valueMap)

    val v = valueMap.get(checkbox.label)
    val all = v.isDefined && (v.get.equalsIgnoreCase("true") || v.get.equalsIgnoreCase("on"))
    val instPK = {
      if (all || isWhitelisted) None
      else {
        val user = CachedUser.get(valueMap(userIdRealTag)).get
        Some(user.institutionPK)
      }
    }

    val requestSeq = getRequestedSeq(valueMap)
    if (requestSeq.nonEmpty) {
      Output.extendedList(requestSeq.toSet)
    } else
      Output.extendedList(instPK, entriesPerPage)
  }

  override def getPK(extendedValues: Output.ExtendedValues): Long = extendedValues.output_outputPK

  override val canCreate: Boolean = false

  //noinspection SpellCheckingInspection
  override def makeRunScript(): Option[String] = Some(s"""
      |
      |function StatusChange(newValue, outputPK) {
      |
      |  var xhttp = new XMLHttpRequest();
      |
      |    xhttp.onreadystatechange = function() {
      |    if (this.readyState == 4 && this.status == 200) {
      |      alert(this.responseText);
      |    }
      |  };
      |
      |  if (newValue == "delete") {
      |    var url = "/view/OutputList?delete=" + outputPK;
      |    location.replace("/view/OutputList?delete=" + outputPK);
      |  }
      |  else {
      |    xhttp.open("POST", "/view/OutputList?$statusTag=" + newValue + "&$outputPKTag=" + outputPK, true);
      |    xhttp.send();
      |   }
      | }
      |
      |""".stripMargin)

  /**
    * Determine if user is authorized to perform delete.  To be authorized, the user must be from the
    * same institution as the original user or be whitelisted.
    */
  private def userAuthorizedToDelete(request: Request, output: Output): Boolean = {

    def sameInstitution: Boolean = {
      val user = CachedUser.get(request).get
      val input = Input.get(output.inputPK).get
      val mach = Machine.get(input.machinePK.get).get
      val dataInstitution = mach.institutionPK
      val requestersInstitution = user.institutionPK
      val same = dataInstitution == requestersInstitution
      logger.info("user requesting delete.  Authorized: " + same)
      same
    }

    val isAuth = userIsWhitelisted(request) || sameInstitution
    isAuth
  }

  //  override def get(valueMap: ValueMapT, response: Response) = {
  //    val form = new WebForm(listPath, List(new WebRow(titleRow(valueMap)) ++ htmlFieldList(valueMap) ++ new WebRow(tableRow(valueMap, response))))
  //    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  //  }

  /**
    * Tell the user that the redo is forbidden and why.  Also give them a redirect back to the list of results.
    */
  private def showNotAuthorizedToDelete(response: Response, outputPK: Long): Unit = {
    val msg = "Only users who are from the same institution may delete."
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-2">
          Only users who are from the same institution may delete records.
          <p></p>
          <a href={OutputList.path} class="btn btn-default" role="button">Back</a>
        </div>
      </div>
    }

    logger.info(msg + "  outputPK: " + outputPK)
    val text = wrapBody(content, "Delete not permitted")
    setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
  }

  /**
    * Allow user to confirm delete.
    */
  private def showConfirmDelete(response: Response, outputPK: Long): Unit = {
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-2">
          Click Confirm to delete, Cancel to return to the list without deleting.
          <p></p>
          <div class="row">
            <div class="col-md-2 col-md-offset-2">
              <a href={OutputList.path} class="btn btn-default" role="button">Cancel</a>
            </div>
            <div class="col-md-2">
              <a href={OutputList.path + "?" + OutputList.deleteTag + "=" + outputPK + "&" + OutputList.confirmTag + "=true"} class="btn btn-danger" role="button">Confirm</a>
            </div>
          </div>
        </div>
      </div>
    }

    logger.info("User was shown delete confirmation for outputPK: " + outputPK)
    val text = wrapBody(content, "Confirm Delete")
    setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
  }

  /** Remove the output, the other associated outputs, and the directory. If its input is only referenced by this output, then delete the input too. */
  private def deleteOutput(outputPK: Long, response: Response): Unit = {

    try {
      val output = Output.get(outputPK)
      Output.delete(output.get.outputPK.get)
      val list = Output.listByInputPK(output.get.inputPK)
      if (list.isEmpty) {
        val input = Input.get(output.get.inputPK)
        Input.delete(input.get.inputPK.get)
        Util.deleteFileTreeSafely(input.get.dir)
      } else Util.deleteFileTreeSafely(output.get.dir)
      OutputList.redirect(response)
    } catch {
      case t: Throwable => internalFailure(response, "Unexpected error in OutputList.deleteOutput: " + fmtEx(t))
    }
  }

  private def noSuchOutput(response: Response): Unit = {
    response.setEntity("No such output.  Most likely it has been deleted or redone.  Refresh the page for the latest list of outputs.", MediaType.TEXT_PLAIN)
    val content = {
      <div>No such output.  Possibly it has been deleted or redone.  Refresh the page for the latest list of outputs.</div>
    }
    simpleWebPage(content, Status.CLIENT_ERROR_BAD_REQUEST, "No such output", response)
  }

  private def handleDelete(valueMap: ValueMapT, response: Response): Int = {
    val outputPK = valueMap(deleteTag).toLong
    Output.get(outputPK) match {
      case None =>
        noSuchOutput(response)
        Filter.SKIP
      case Some(output) =>
        if (userAuthorizedToDelete(response.getRequest, output)) {
          if (valueMap.contains(OutputList.confirmTag)) {
            deleteOutput(outputPK, response)
            Filter.SKIP
          } else {
            showConfirmDelete(response, outputPK)
            Filter.STOP
          }
        } else {
          showNotAuthorizedToDelete(response, outputPK)
          Filter.SKIP
        }
    }
  }

  /**
    * Redo the given output.
    */
  def redoOutput(outputPK: Long, response: Response, await: Boolean = false, isAuto: Boolean = false, authenticatedUserPK: Option[Long], sync: Boolean): Unit = {

    Output.get(outputPK) match {
      case None => noSuchOutput(response)
      case Some(output) =>
        Output.ensureInputAndOutputFilesExist(output)
        val procedure = Procedure.get(output.procedurePK).get
        logger.info("Starting redo of output " + output + "    procedure: " + procedure)
        val runTrait = WebRun.get(output.procedurePK).right.get.asInstanceOf[RunTrait[RunReqClass]]
        // Seems a bit round-about to create the valueMap, but this handles the bulk redo case.
        val valueMap = Map((OutputList.redoTag, output.outputPK.get.toString), (WebUtil.awaitTag, await.toString), (WebUtil.autoUploadTag, isAuto.toString))
        RunProcedure.handleInput(valueMap, response, runTrait, authenticatedUserPK, sync)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  private def startBulkRedo(valueMap: ValueMapT, response: Response): Unit =
    OutputList.path.synchronized {
      val outputPkList = getRequestedSeq(valueMap)
      logger.info("Performing bulk redo on: " + outputPkList.mkString(" "))

      def redoOne(output: Output): Unit = {
        try {
          if (OutputList.bulkRedoIsRunning.get())
            logger.info("running state: " + OutputList.bulkRedoIsRunning.get())
          while (!OutputList.bulkRedoIsRunning.get())
            Thread.sleep(2000)

          val outputPK = output.outputPK.get
          logger.info("Performing bulk redo member: " + outputPK)
          val start = System.currentTimeMillis
          redoOutput(outputPK, response, await = true, isAuto = true, authenticatedUserPK = output.userPK, sync = true)
          val elapsed = System.currentTimeMillis - start
          logger.info("Performed bulk redo member: " + outputPK + " in " + elapsed + " ms")
          // Wait in case another user is trying to execute a procedure.
          Thread.sleep(4 * 1000)
        } catch {
          case t: Throwable => logger.error("unexpected error redoing output " + output + " : " + fmtEx(t))
        }
      }

      outputPkList.flatMap(Output.get).map(redoOne)
    }

  /**
    * Examine the valueMap and determine if this is a status change of an output.
    * @param valueMap Parameters from URL.
    * @return True if it is a status change.
    */
  private def isStatusChange(valueMap: WebUtil.ValueMapT): Boolean = {
    val is = valueMap.contains(statusTag) && valueMap.contains(outputPKTag)
    is
  }

  private def changeStatus(valueMap: WebUtil.ValueMapT, response: Response): Int = {

    val status = valueMap(statusTag)
    val outputPK = valueMap(outputPKTag).toLong
    logger.info(s"Changing status of outputPK $outputPK to $status ...")

    if (status.equals(deleteTag)) {
      handleDelete(valueMap, response)
    } else {
      val output = Output.get(outputPK).get

      val newDataValidity = validityList.find(_.equals(status)).get // Make sure that the new status is supported.
      val newOutput = output.copy(dataValidity = newDataValidity)
      newOutput.insertOrUpdate()
      logger.info(s"Changed status of outputPK $outputPK to $status ...")

      response.setStatus(Status.SUCCESS_OK)
      response.setEntity(s"Status changed to $newDataValidity", MediaType.TEXT_PLAIN)
      Filter.STOP
    }
  }

  override def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Int = {
    try {
      val delete = valueMap.get(OutputList.deleteTag)
      val redo = valueMap.get(OutputList.redoTag)

      0 match {
        case _ if delete.isDefined =>
          handleDelete(valueMap, response)
          Filter.SKIP
        case _ if redo.isDefined =>
          redoOutput(redo.get.toLong, response, authenticatedUserPK = None, sync = true)
          Filter.SKIP
        case _ if buttonIs(valueMap, stopButton) =>
          OutputList.bulkRedoIsRunning.set(false)
          Filter.CONTINUE
        case _ if buttonIs(valueMap, resumeButton) =>
          OutputList.bulkRedoIsRunning.set(true)
          Filter.CONTINUE
        case _ if isStatusChange(valueMap) =>
          changeStatus(valueMap, response)
        case _ =>
          if (buttonIs(valueMap, redoAll)) {
            Future { startBulkRedo(valueMap, response) }
            logger.info("Bulk redo started.")
          }
          Filter.CONTINUE
      }
    } catch {
      case t: Throwable =>
        internalFailure(response, "Unexpected error in OutputList: " + fmtEx(t))
        Filter.STOP
    }
  }
}
