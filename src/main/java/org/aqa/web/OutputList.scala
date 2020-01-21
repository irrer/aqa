package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import java.text.SimpleDateFormat
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.Institution
import org.restlet.Request
import org.aqa.web.WebUtil._
import edu.umro.util.Utility
import org.aqa.webrun.phase2.Phase2
import org.aqa.db.CachedUser
import org.restlet.data.Status
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.bbByCBCT.BBbyCBCTRun
import org.aqa.webrun.bbByEpid.BBbyEPIDRun
import org.restlet.routing.Filter
import org.aqa.Util
import org.aqa.AnonymizeUtil

/**
 * List the outputs to let users re-visit results.
 */

object OutputList {
  val deleteTag = "delete"
  val redoTag = "redo"
  val confirmTag = "confirm"

  val path = new String((new OutputList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class OutputList extends GenericList[Output.ExtendedValues] with WebUtil.SubUrlView {

  override def listName = "Output"

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show output from all institutions, then click 'Refresh'"), 2, 0)
  val refresh = makeButton("Refresh", ButtonType.BtnPrimary)

  private def getDoneHtml(valueMap: ValueMapT): Elem = {
    val redoSet = getRedoSet(valueMap)
    val response: Response = null // this never gets used
    val existing = getData(valueMap, response).map(e => e.output_outputPK).toSet
    val donePkSet = redoSet.diff(existing)
    val doneText = redoSet.diff(existing).toSeq.sorted.mkString("   ")
    <div title="List of outputPK's that no longer exist or never existed">{ doneText }</div>
  }

  def bulkRedoInstructions = {
    val title =
      "To perform a bulk redo, enter a list of outputPKs in the 'To Do' box." + titleNewline +
        "Clicking the Refresh button will refresh the list containing just the 'To Do' entries." + titleNewline +
        "outputPKs may be separated by any whitespace, comma, or non-digit." + titleNewline +
        "If the 'To Do' list is empty then all outputs will be listed." + titleNewline +
        "Output redo's are performed sequentially so as not to overload the server." + titleNewline +
        "The page will refresh when all of the outputs have been performed." + titleNewline +
        "Once started, the only way to stop processing the list is to restart the server." + titleNewline +
        "You may monitor progress by copying and pasting list to another results" + titleNewline +
        "page and refreshing that screen." + titleNewline

    val elem = {
      <div title={ title }>Hover here for bulk redo instructions</div>
    }
    new WebPlainText("Bulk Redo Instructions", false, 6, 0, (ValueMapT) => elem)
  }

  val todoList = new WebInputTextArea("To Do", 3, 0, "List of Output PK's to Redo")
  val doneList = new WebPlainText("Done", true, 3, 0, getDoneHtml)
  val redoAll = makeButton("Redo All", ButtonType.BtnDefault)

  override def htmlFieldList(valueMap: ValueMapT): List[WebRow] = {
    val webRow = List(checkbox, refresh)
    def instructRow = List(bulkRedoInstructions)
    def redoRow = List(todoList, doneList, redoAll)

    if (userIsWhitelisted(valueMap))
      List(webRow, instructRow, redoRow)
    else
      List(webRow)
  }

  private def humanReadableURL(url: String): String = {
    val small = url.replaceAll("^https://", "").replaceAll("^http://", "").replaceAll("^www\\.", "")
    WebUtil.firstPartOf(small, 20)
  }

  private def procedureName(output: Output): String = Procedure.get(output.procedurePK).get.fullName

  private def machineName(output: Output): String = {
    try {
      val input = Input.get(output.outputPK.get)
      val machine = Machine.get(input.get.machinePK.get)
      machine.get.id
    } catch {
      case e: Exception => "none"
    }
  }

  private def institutionName(output: Output): String = {
    try {
      val input = Input.get(output.outputPK.get)
      val machine = Machine.get(input.get.machinePK.get)
      val institution = Institution.get(machine.get.institutionPK)
      institution.get.name
    } catch {
      case e: Exception => "none"
    }
  }

  private def user(output: Output): String = User.get(output.userPK.get).get.fullName_real

  private val startTimeFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss")

  private def compareByInputTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Int = {
    if (a.input_dataDate.isDefined && b.input_dataDate.isDefined) a.input_dataDate.get.compareTo(b.input_dataDate.get)
    else 0
  }

  private def compareByStartTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Boolean = a.output_startDate.compareTo(b.output_startDate) > 0

  private def inputTime(extendedValues: Output.ExtendedValues): String = {
    val date = extendedValues.input_dataDate
    if (date.isDefined) startTimeFormat.format(date.get) else "unknown date"
  }

  private def getUrl(outputPK: Long, summary: Boolean): String = {
    val sum = if (summary) ("&" + ViewOutput.summaryTag + "=true") else ""
    val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + outputPK + sum
    url
  }

  private def startTimeUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Data analysis time" href={ getUrl(extendedValues.output_outputPK, false) }> { startTimeFormat.format(extendedValues.output_startDate) }</a>
  }

  private def inputFileUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Data aquisition time" href={ WebServer.urlOfResultsPath(extendedValues.input_directory.get) }>{ inputTime(extendedValues) }</a>
  }

  private def deleteUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Click to delete.  Can NOT be undone" href={ OutputList.path + "?" + OutputList.deleteTag + "=" + extendedValues.output_outputPK }>Delete</a>
  }

  private def redoUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Click to re-run analysis.  Results will replace previous results." href={ OutputList.path + "?" + OutputList.redoTag + "=" + extendedValues.output_outputPK }>Redo</a>
  }

  type ColT = Output.ExtendedValues // Column Type

  private val institutionCol = new Column[ColT]("Institution", _.institution_name, (colT) => wrapAlias(colT.institution_name))

  private val userCol = new Column[ColT]("User", _.user_id, (colT) => wrapAlias(colT.user_id))

  private val startTimeCol = new Column[ColT]("Analysis", compareByStartTime _, startTimeUrl _)

  private val inputFileCol = new Column[ColT]("Acquisition", inputTime _)

  private val redoCol = new Column[ColT]("Redo", _ => "Redo", redoUrl)

  private val deleteCol = new Column[ColT]("Delete", _ => "Delete", deleteUrl)

  private val procedureCol = new Column[ColT]("Procedure", (d) => d.procedure_name + " " + d.procedure_version)

  private val machineCol = new Column[ColT]("Machine", _.machine_id, (colT) => wrapAlias(colT.machine_id))

  override val columnList = Seq(startTimeCol, inputFileCol, redoCol, procedureCol, machineCol, institutionCol, userCol, deleteCol)

  val entriesPerPage = 4000 // TODO should support pagination

  /**
   * Get the set of outputs that user wants to redo.  Return empty set if none.  This
   * feature is only available to whitelisted users.
   */
  private def getRedoSet(valueMap: ValueMapT): Set[Long] = {
    val isWhitelisted = userIsWhitelisted(valueMap)
    val outPkSet = if (isWhitelisted && valueMap.get(todoList.label).isDefined && valueMap(todoList.label).trim.nonEmpty) {
      // get all integers, distinct and sorted
      valueMap(todoList.label).replaceAll("[^0-9]", " ").split(" ").toSeq.filter(t => t.nonEmpty).map(t => t.toLong).toSet
    } else
      Set[Long]()
    outPkSet
  }

  override def getData(valueMap: ValueMapT, response: Response): Seq[Output.ExtendedValues] = {

    val isWhitelisted = userIsWhitelisted(valueMap)

    val v = valueMap.get(checkbox.label)
    val all = v.isDefined && (v.get.equalsIgnoreCase("true") || v.get.equalsIgnoreCase("on"))
    val instPK = {
      if (all || isWhitelisted) None
      else {
        val userIdReal = valueMap(userIdRealTag)
        val user = CachedUser.get(valueMap(userIdRealTag)).get
        Some(user.institutionPK)
      }
    }

    val redoSet = getRedoSet(valueMap)
    if (redoSet.nonEmpty) {
      Output.extendedList(redoSet)
    } else
      Output.extendedList(instPK, entriesPerPage)
  }

  override def getPK(extendedValues: Output.ExtendedValues): Long = extendedValues.output_outputPK

  override val canCreate: Boolean = false

  /**
   * Determine if user is authorized to perform delete.  To be authorized, the user must be from the
   * same institution as the original user or be whitelisted.
   */
  private def userAuthorizedToDelete(request: Request, response: Response, output: Output): Boolean = {

    def sameInstitution: Boolean = {
      val user = CachedUser.get(request).get
      val input = Input.get(output.inputPK).get
      val mach = Machine.get(input.machinePK.get).get
      val dataInstitution = mach.institutionPK
      val requestorsInstitution = user.institutionPK
      val same = dataInstitution == requestorsInstitution
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
  private def showNotAuthorizedToDelete(response: Response, outputPK: Long) {
    val msg = "Only users who are from the same institution may delete."
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-2">
          Only users who are from the same institution may delete records.
          <p></p>
          <a href={ OutputList.path } class="btn btn-default" role="button">Back</a>
        </div>
      </div>
    }

    logger.info(msg + "  ouputPK: " + outputPK)
    val text = wrapBody(content, "Delete not permitted")
    setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
  }

  /**
   * Allow user to confirm delete.
   */
  private def showConfirmDelete(response: Response, outputPK: Long) {
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-2">
          Click Confirm to delete, Cancel to return to the list without deleting.
          <p></p>
          <div class="row">
            <div class="col-md-2 col-md-offset-2">
              <a href={ OutputList.path } class="btn btn-default" role="button">Cancel</a>
            </div>
            <div class="col-md-2">
              <a href={ OutputList.path + "?" + OutputList.deleteTag + "=" + outputPK + "&" + OutputList.confirmTag + "=true" } class="btn btn-danger" role="button">Confirm</a>
            </div>
          </div>
        </div>
      </div>
    }

    logger.info("User was shown delete confirmation for ouputPK: " + outputPK)
    val text = wrapBody(content, "Confirm Delete")
    setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
  }

  /** Remove the output, the other associated outputs, and the directory. If its input is only referenced by this output, then delete the input too. */
  private def deleteOutput(outputPK: Long, response: Response): Unit = {

    try {
      val output = Output.get(outputPK)
      Output.delete(output.get.outputPK.get)
      val list = Output.listByInputPK(output.get.inputPK)
      if (list.size == 0) {
        val input = Input.get(output.get.inputPK)
        Input.delete(input.get.inputPK.get)
        Util.deleteFileTreeSafely(input.get.dir)
      } else Util.deleteFileTreeSafely(output.get.dir)
      OutputList.redirect(response)
    } catch {
      case t: Throwable => internalFailure(response, "Unexpected error in OutputList.deleteOutput: " + fmtEx(t))
    }
  }

  private def handleDelete(valueMap: ValueMapT, request: Request, response: Response) = {

    val outputPK = valueMap.get(OutputList.deleteTag).get.toLong
    val output = Output.get(outputPK).get
    if (userAuthorizedToDelete(response.getRequest, response, output)) {
      if (valueMap.get(OutputList.confirmTag).isDefined) {
        deleteOutput(outputPK, response)
      } else
        showConfirmDelete(response, outputPK)
    } else
      showNotAuthorizedToDelete(response, outputPK)
  }

  private def redoOutput(outputPK: Long, response: Response, await: Boolean = false, isAuto: Boolean = false): Unit = {
    Output.get(outputPK) match {
      case None => ;
      case Some(output) => {
        Output.ensureInputAndOutputFilesExist(output)
        val procedure = Procedure.get(output.procedurePK).get
        if (procedure.name.toLowerCase.contains("phase")) {
          Phase2.redo(outputPK, response.getRequest, response, await, isAuto)
        }
        if (procedure.name.toLowerCase.contains("cbct")) {
          BBbyCBCTRun.redo(outputPK, response.getRequest, response, await, isAuto)
        }
        if (procedure.name.toLowerCase.contains("epid")) {
          BBbyEPIDRun.redo(outputPK, response.getRequest, response, await, isAuto)
        }
      }
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private def startBulkRedo(valueMap: ValueMapT, response: Response): Unit = OutputList.path.synchronized {
    val outputPkList = getRedoSet(valueMap).toSeq.sorted
    logger.info("Performing bulk redo on: " + outputPkList.mkString(" "))
    outputPkList.map(outputPK => {

      val output = Output.get(outputPK).get
      val machine = Machine.get(output.machinePK.get).get
      val institutionPK = machine.institutionPK
      val user = User.getOrMakeInstitutionAdminUser(institutionPK)
      val userId = AnonymizeUtil.decryptWithNonce(institutionPK, user.id_real.get)
      val request = response.getRequest
      val cr = request.getChallengeResponse
      cr.setSecret("secret password")
      cr.setIdentifier(userId)
      request.setChallengeResponse(cr)
      val u1 = getUser(request) // TODO rm
      response.setRequest(request)
      val effectiveUser = WebUtil.getUser(request)

      val u2 = getUser(request) // TODO rm
      logger.info("Performing bulk redo member: " + outputPK)
      val start = System.currentTimeMillis
      redoOutput(outputPK, response, true, true)
      val elapsed = System.currentTimeMillis - start
      logger.info("Performed bulk redo member: " + outputPK + " in " + elapsed + " ms")
    })

  }

  override def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Int = {
    try {
      val delete = valueMap.get(OutputList.deleteTag)
      val redo = valueMap.get(OutputList.redoTag)

      0 match {
        case _ if (delete.isDefined) => {
          handleDelete(valueMap, request, response)
          Filter.SKIP
        }
        case _ if (redo.isDefined) => {
          redoOutput(redo.get.toLong, response)
          Filter.SKIP
        }
        case _ => {
          if (buttonIs(valueMap, redoAll)) {
            startBulkRedo(valueMap, response)
          }
          Filter.CONTINUE
        }
      }
    } catch {
      case t: Throwable =>
        internalFailure(response, "Unexpected error in OutputList: " + fmtEx(t))
        Filter.STOP
    }
  }
}
