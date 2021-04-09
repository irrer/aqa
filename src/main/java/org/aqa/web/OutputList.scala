package org.aqa.web

import edu.umro.ScalaUtil.Trace
import org.aqa.AnonymizeUtil
import org.aqa.Crypto
import org.aqa.Util
import org.aqa.db.CachedUser
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil._
import org.aqa.webrun.WebRun
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.routing.Filter

import java.text.SimpleDateFormat
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Elem

/**
  * List the outputs to let users re-visit results.
  */

object OutputList {
  val deleteTag = "delete"
  val redoTag = "redo"
  val confirmTag = "confirm"

  val path = new String((new OutputList).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)

  def redoUrl(outputPK: Long): Elem = {
    <a title="Click to re-run analysis. New results will replace previous results." href={path + "?" + redoTag + "=" + outputPK}>Redo</a>
  }

}

class OutputList extends GenericList[Output.ExtendedValues] with WebUtil.SubUrlView {

  override def listName = "Output"

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    val action = pathOf + "?" + name + "=" + name
    new FormButton(name, 1, 0, subUrl, action, buttonType)
  }

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show output from all institutions, then click 'Refresh'"), 2, 0)
  val refresh: FormButton = makeButton("Refresh", ButtonType.BtnPrimary)

  private def getToDoHtml(valueMap: ValueMapT): Elem = {
    val requestSet = getRequestedSet(valueMap)
    val todoText =
      if (requestSet.isEmpty)
        ""
      else {
        val response: Response = null // this never gets used
        val todoSet = getData(valueMap, response).map(e => e.output_outputPK).toSet
        todoSet.toSeq.sorted.mkString(" ")
      }
    <div title="List of outputPK's need to be done">{todoText}</div>
  }

  private def getDoneHtml(valueMap: ValueMapT): Elem = {
    val requestSet = getRequestedSet(valueMap)
    val response: Response = null // this never gets used
    val existing = getData(valueMap, response).map(e => e.output_outputPK).toSet
    // val donePkSet = requestSet.diff(existing)
    val doneText = requestSet.diff(existing).toSeq.sorted.mkString("   ")
    <div title="List of outputPK's that no longer exist or never existed">{doneText}</div>
  }

  def bulkRedoInstructions: WebPlainText = {
    val title =
      "To perform a bulk redo, enter a list of outputPKs in the 'Request List' box." + titleNewline +
        "Clicking the Refresh button will refresh the list containing just the 'Request List' entries." + titleNewline +
        "outputPKs may be separated by any whitespace, comma, or non-digit." + titleNewline +
        "If the 'Request List' list is empty then all outputs will be listed." + titleNewline +
        "Output redo's are performed sequentially so as not to overload the server." + titleNewline +
        "The page will refresh when all of the outputs have been performed." + titleNewline +
        "Once started, the only way to stop processing the list is to restart the server." + titleNewline +
        "You may monitor progress by copying and pasting list to another results" + titleNewline +
        "page and refreshing that screen." + titleNewline

    def elem(valueMap: ValueMapT) = {
      val quantities: Elem = {
        val requestList = getRequestedSet(valueMap).toSeq.sorted
        if (requestList.isEmpty)
          <div></div>
        else {
          val response: Response = null // this never gets used
          val todoSet = getData(valueMap, response).map(e => e.output_outputPK)
          <div class="row">
            <div class="col-md-2">Requested: {requestList.size}</div>
            <div class="col-md-2">To Do: {todoSet.size}</div>
            <div class="col-md-2">Done: {(requestList.size - todoSet.size).toString}</div>
          </div>
        }
      }
      <div title={title}>Hover here for bulk redo instructions.  {quantities}</div>
    }
    new WebPlainText("Bulk Redo Instructions", false, 6, 0, (valueMap: ValueMapT) => elem(valueMap))
  }

  val requestList = new WebInputTextArea("Request List", 4, 0, "List of Output PK's to Redo")
  val todoList = new WebPlainText("To Do", true, 4, 0, getToDoHtml)
  val doneList = new WebPlainText("Done", true, 4, 0, getDoneHtml)
  val redoAll: FormButton = makeButton("Redo All", ButtonType.BtnDefault)

  override def htmlFieldList(valueMap: ValueMapT): List[WebRow] = {
    val webRow = List(checkbox, refresh)
    def instructRow = List(bulkRedoInstructions, redoAll)
    def redoRow = List(requestList, todoList, doneList)

    if (userIsWhitelisted(valueMap))
      List(webRow, instructRow, redoRow)
    else
      List(webRow)
  }

  /*
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
   */

  // private def user(output: Output): String = User.get(output.userPK.get).get.fullName_real

  private val startTimeFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss")

  /*
  private def compareByInputTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Int = {
    if (a.input_dataDate.isDefined && b.input_dataDate.isDefined) a.input_dataDate.get.compareTo(b.input_dataDate.get)
    else 0
  }
   */

  //noinspection SameParameterValue
  private def compareByStartTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Boolean = a.output_startDate.compareTo(b.output_startDate) > 0

  private def inputTime(extendedValues: Output.ExtendedValues): String = {
    val date = extendedValues.input_dataDate
    if (date.isDefined) startTimeFormat.format(date.get) else "unknown date"
  }

  private def getUrl(outputPK: Long, summary: Boolean): String = {
    val sum = if (summary) "&" + ViewOutput.summaryTag + "=true" else ""
    val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + outputPK + sum
    url
  }

  private def startTimeUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Data analysis time" href={getUrl(extendedValues.output_outputPK, summary = false)}> {startTimeFormat.format(extendedValues.output_startDate)}</a>
  }

  /*
  private def inputFileUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Data aquisition time" href={WebServer.urlOfResultsPath(extendedValues.input_directory.get)}>{inputTime(extendedValues)}</a>
  }
   */

  private def deleteUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Click to delete.  Can NOT be undone" href={OutputList.path + "?" + OutputList.deleteTag + "=" + extendedValues.output_outputPK}>Delete</a>
  }

  private def redoUrl(extendedValues: Output.ExtendedValues): Elem = {
    OutputList.redoUrl(extendedValues.output_outputPK)
  }

  type ColT = Output.ExtendedValues // Column Type

  private val institutionCol = new Column[ColT]("Institution", _.institution_name, colT => wrapAlias(colT.institution_name))

  private val userCol = new Column[ColT]("User", _.user_id, colT => wrapAlias(colT.user_id))

  private val startTimeCol = new Column[ColT]("Analysis", compareByStartTime _, startTimeUrl _)

  private val inputFileCol = new Column[ColT]("Acquisition", inputTime)

  private val redoCol = new Column[ColT]("Redo", _ => "Redo", redoUrl)

  private val deleteCol = new Column[ColT]("Delete", _ => "Delete", deleteUrl)

  private val procedureCol = new Column[ColT]("Procedure", d => d.procedure_name + " " + d.procedure_version)

  private val machineCol = new Column[ColT]("Machine", _.machine_id, colT => wrapAlias(colT.machine_id))

  override val columnList = Seq(startTimeCol, inputFileCol, redoCol, procedureCol, machineCol, institutionCol, userCol, deleteCol)

  val entriesPerPage = 600 // TODO should support pagination

  /**
    * Get the set of outputs that user wants to redo.  Return empty set if none.  This
    * feature is only available to whitelisted users.
    */
  private def getRequestedSet(valueMap: ValueMapT): Set[Long] = {
    val isWhitelisted = userIsWhitelisted(valueMap)
    val outPkSet = if (isWhitelisted && valueMap.contains(requestList.label) && valueMap(requestList.label).trim.nonEmpty) {
      // get all integers, distinct and sorted
      valueMap(requestList.label).replaceAll("[^0-9]", " ").split(" ").toSeq.filter(t => t.nonEmpty).map(t => t.toLong).toSet
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
        val user = CachedUser.get(valueMap(userIdRealTag)).get
        Some(user.institutionPK)
      }
    }

    val requestSet = getRequestedSet(valueMap)
    if (requestSet.nonEmpty) {
      Output.extendedList(requestSet)
    } else
      Output.extendedList(instPK, entriesPerPage)
  }

  override def getPK(extendedValues: Output.ExtendedValues): Long = extendedValues.output_outputPK

  override val canCreate: Boolean = false

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
  private def showNotAuthorizedToDelete(response: Response, outputPK: Long) {
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
  private def showConfirmDelete(response: Response, outputPK: Long) {
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
    response.setEntity("No such output.  Most likely it has been deleted or redone.  Refresh the page for the latest list of outputs.", MediaType.TEXT_PLAIN) // TODO
    val content = {
      <div>No such output.  Possibly it has been deleted or redone.  Refresh the page for the latest list of outputs.</div>
    }
    simpleWebPage(content, Status.CLIENT_ERROR_BAD_REQUEST, "No such output", response)
  }

  private def handleDelete(valueMap: ValueMapT, response: Response): Unit = {

    val outputPK = valueMap(OutputList.deleteTag).toLong
    Output.get(outputPK) match {
      case None => noSuchOutput(response)
      case Some(output) =>
        if (userAuthorizedToDelete(response.getRequest, output)) {
          if (valueMap.contains(OutputList.confirmTag)) {
            deleteOutput(outputPK, response)
          } else
            showConfirmDelete(response, outputPK)
        } else
          showNotAuthorizedToDelete(response, outputPK)
    }
  }

  /**
    * Redo the given output.
    */
  def redoOutput(outputPK: Long, response: Response, await: Boolean = false, isAuto: Boolean = false): Unit = {
    Output.get(outputPK) match {
      case None => noSuchOutput(response)
      case Some(output) =>
        Output.ensureInputAndOutputFilesExist(output)
        val procedure = Procedure.get(output.procedurePK).get
        logger.info("Starting redo of output " + output + "    procedure: " + procedure)
        val runTrait = WebRun.get(output.procedurePK).right.get.asInstanceOf[RunTrait[RunReqClass]]
        // Seems a bit round-about to create the valueMap, but this handles the bulk redo case.
        val valueMap = Map((OutputList.redoTag, output.outputPK.get.toString), (WebUtil.awaitTag, await.toString), (WebUtil.autoUploadTag, isAuto.toString))
        RunProcedure.handle(valueMap, response.getRequest, response, runTrait)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  /**
    * Set up the given response+request with a user from the given institution that can be used to do a redo.
    * @param institutionPK User must belong to this institution.
    * @param response Put credentials here.
    */
  private def setupAdminUser(institutionPK: Long, response: Response): Unit = {
    val passwordText = Crypto.makeRandomCipherKey

    // Find an admin user for the given institution.  If one does not exist, then make one.  Either way this
    // user is in the database.
    val dbUser = User.getOrMakeInstitutionAdminUser(institutionPK)

    // The real (in the clear) user id
    val userId = AnonymizeUtil.decryptWithNonce(institutionPK, dbUser.id_real.get)

    // make a temporary in-memory copy of the user with a password we know, then put it in the cache so it will be
    // valid for a short time.

    Trace.trace("response.getRequest.getChallengeResponse.getIdentifier : " + response.getRequest.getChallengeResponse.getIdentifier) // TODO rm
    Trace.trace("response.getRequest.getChallengeResponse.getSecret     : " + new String(response.getRequest.getChallengeResponse.getSecret)) // TODO rm
    if (CachedUser.get(response).isEmpty) // TODO rm
      Trace.trace("gonna fail!") // TODO rm
    def thisUser = CachedUser.get(response).get
    def thisUserId = response.getRequest.getChallengeResponse.getIdentifier

    val passwordSalt = Crypto.randomSecureHash
    val hashedPassword = CachedUser.hashPassword(passwordText, passwordSalt)
    val user = dbUser.copy(passwordSalt = passwordSalt, hashedPassword = hashedPassword)
    CachedUser.clear
    CachedUser.put(userId, user)
    CachedUser.put(thisUserId, thisUser)

    // put the user id and password into the request so it has the authority to do a redo
    val request = response.getRequest
    val cr = request.getChallengeResponse
    cr.setSecret(passwordText)
    cr.setIdentifier(userId)
    request.setChallengeResponse(cr)
    response.setRequest(request)
  }

  private def startBulkRedo(valueMap: ValueMapT, response: Response): Unit =
    OutputList.path.synchronized {
      val outputPkList = getRequestedSet(valueMap).toSeq.sorted
      logger.info("Performing bulk redo on: " + outputPkList.mkString(" "))

      def redoOne(outputPK: Long): Unit = {
        try {
          val output = Output.get(outputPK).get
          val machine = Machine.get(output.machinePK.get).get
          setupAdminUser(machine.institutionPK, response)
          logger.info("Performing bulk redo member: " + outputPK)
          val start = System.currentTimeMillis
          redoOutput(outputPK, response, await = true, isAuto = true)
          val elapsed = System.currentTimeMillis - start
          logger.info("Performed bulk redo member: " + outputPK + " in " + elapsed + " ms")
        } catch {
          case t: Throwable => logger.error("unexpected error redoing output " + outputPK + " : " + fmtEx(t))
        }
      }

      outputPkList.map(outputPK => redoOne(outputPK))

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
          redoOutput(redo.get.toLong, response)
          Filter.SKIP
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
