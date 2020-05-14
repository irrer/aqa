package org.aqa.run

import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.OutputList
import edu.umro.util.Utility
import org.aqa.web.WebRunIndex
import org.restlet.data.Status
import com.pixelmed.dicom.AttributeList
import java.util.Date
import java.sql.Timestamp
import org.aqa.db.Input
import org.aqa.db.Procedure
import java.io.File
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.Institution
import org.aqa.Util
import org.aqa.Config
import org.aqa.web.WebUtil
import org.aqa.db.Output
import org.aqa.web.WebServer
import org.aqa.db.DataValidity
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.aqa.db.CachedUser
import org.aqa.db.DicomSeries
import org.aqa.webrun.ExtendedData
import org.aqa.web.ViewOutput
import sys.process._
import scala.sys.process._
import scala.concurrent._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import org.aqa.DicomFile

object RunProcedure extends Logging {

  private val runButtonName = "Run"
  private val cancelButtonName = "Cancel"

  private val outputSubdirNamePrefix = "output_"
  val machineSelectorLabel = "Machine"

  def makeForm(runTrait: RunTrait[RunReqClass]) = {
    val machineSelector = new WebInputSelectMachine(machineSelectorLabel, 6, 0)

    def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
      val action = runTrait.getProcedure.webUrl + "?" + name + "=" + name
      new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    val runButton = makeButton(runButtonName, true, ButtonType.BtnDefault)
    val cancelButton = makeButton(cancelButtonName, false, ButtonType.BtnDefault)

    val form = new WebForm(runTrait.getProcedure.webUrl, Some("BBbyEPID"), List(List(machineSelector), List(runButton, cancelButton)), 10)
    form
  }

  /**
   * Make the string nice for using as a file name, replacing invalid characters (like /) with _, and all blanks with _, and appending the given primary key.
   */
  private def niceifyAndAppendPK(text: String, pk: Long): String = makeValidName(text + "_" + pk.toString)

  private def makeValidName(text: String): String = FileUtil.replaceInvalidFileNameCharacters(text, '_').replace(' ', '_')

  private def institutionFileName(machine: Machine): String = {
    val institution = Institution.get(machine.institutionPK)
    if (institution.isDefined) makeValidName(institution.get.fileName)
    else {
      logger.warn("Run.dir Could not find institution for machine " + machine.toString)
      "unknown_institution_" + machine.fileName
    }
  }

  /**
   * Construct a directory organized by:  institution / machine / procedure / time
   */
  private def makeInputDir(machine: Machine, procedure: Procedure, inputPK: Long): File = {
    def nameHierarchy = List(
      institutionFileName(machine),
      makeValidName(machine.fileName),
      niceifyAndAppendPK(procedure.fileName, procedure.procedurePK.get),
      niceifyAndAppendPK(Util.currentTimeAsFileName, inputPK))

    val inputDir: File = nameHierarchy.foldLeft(Config.resultsDirFile)((d, name) => new File(d, name))

    logger.info("New input directory: " + inputDir.getAbsolutePath)
    inputDir
  }

  /**
   * Move all of the files from the old to the new directory.
   */
  private def renameFileTryingPersistently(oldDir: File, newDir: File): Boolean = {
    logger.info("Attempting to rename file from : " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath)

    // This is fast, but only works if the new and old files are in the same disk partition.
    def renameUsingOldIo: Boolean = {
      try {
        val status = oldDir.renameTo(newDir)
        if (status) logger.info("Used File.renameTo to successfully rename from : " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath)
        status
      } catch {
        case t: Throwable => {
          logger.warn("Failed to rename file with File.renameTo from : " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath + " : " + fmtEx(t))
          false
        }
      }
    }

    def renameUsingNio: Boolean = {
      val retryLimitMs = 2 * 1000
      val timeout = System.currentTimeMillis + retryLimitMs

      val oldPath = java.nio.file.Paths.get(oldDir.getAbsolutePath)
      val newPath = java.nio.file.Paths.get(newDir.getAbsolutePath)
      try {
        val path = java.nio.file.Files.move(oldPath, newPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        logger.info("Used nio to successfully rename from : " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath)
        true
      } catch {
        case t: Throwable => {
          if (System.currentTimeMillis < timeout) {
            logger.warn("Failed to rename file with nio - retrying. From : " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath + " : " + fmtEx(t))
            Thread.sleep(500)
            renameUsingNio
          } else {
            logger.error("Unable using nio to rename file " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath + " : " + fmtEx(t))
            false
          }
        }
      }
    }

    def deleteLater(f: File): Unit = {
      class DeleteLater(file: File) extends Runnable {
        val timeout = System.currentTimeMillis + (60 * 60 * 1000) // try for up to an hour
        override def run: Unit = {
          while ((System.currentTimeMillis < timeout) && (f.exists)) {
            Thread.sleep(20 * 1000)
            Util.deleteFileTreeSafely(f)
          }
          if (f.exists) logger.info("Was able to remove file " + f.getAbsolutePath)
          else logger.warn("Was not able to remove file " + f.getAbsolutePath)
        }

      }
      new Thread((new DeleteLater(f))).start
    }

    def copyFilesAndDeleteLater: Boolean = {
      try {
        Utility.copyFileTree(oldDir, newDir)
        logger.info("Used copyFileTree to successfully copy from : " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath)
        deleteLater(oldDir)
        true
      } catch {
        case t: Throwable => {
          logger.error("Unable using nio to rename file " + oldDir.getAbsolutePath + " to " + newDir.getAbsolutePath + " : " + fmtEx(t))
          false
        }
      }
    }

    newDir.getParentFile.mkdirs
    if (renameUsingOldIo) true
    else {
      if (renameUsingNio) true
      else {
        copyFilesAndDeleteLater
      }
    }
  }

  /**
   * Remove the output, which removes data pointing to it.  Also remove the corresponding output directory.
   */
  private def removeOutput(output: Output): Unit = {
    try {
      Output.delete(output.outputPK.get)
      Util.deleteFileTreeSafely(output.dir)
      logger.info("Removed output " + output)
    } catch {
      case t: Throwable =>
        logger.warn("removeOutput Unexpected error removing output.  oldOutput: " + output + " : " + t.getMessage)
    }
  }

  /**
   * Remove an input and its directory, which will include its child directories (including Output directories).
   */
  private def removeInput(inputPK: Long): Unit = {
    Input.get(inputPK) match {
      case Some(input) => {
        Util.deleteFileTreeSafely(input.dir)
        logger.info("Deleted input dir " + input.dir + " and its child output dirs.")
        Input.delete(inputPK)
      }
      case _ => ;
    }
  }

  /**
   * Construct <code>File</code> and create needed directories for output directory.
   */
  private def makeOutputDir(inputDir: File, outputStartDate: Date): File = {
    val file = new File(inputDir, outputSubdirNamePrefix + Util.timeAsFileName(outputStartDate))
    file.mkdirs
    file
  }

  private def getMachineFromSelector: Option[Machine] = {
    ???
  }

  /**
   * Make sure that all of the DICOM series are saved in the database.  It is quite possible that the same series will
   * be submitted more than once, and in those cases nothing will be done.
   */
  private def saveDicomSeries(userPK: Long, inputPK: Option[Long], machinePK: Option[Long], alList: Seq[AttributeList]): Unit = {

    def insertRtplanIfNew(rtplan: AttributeList): Unit = {
      val existing = DicomSeries.getBySopInstanceUID(Util.sopOfAl(rtplan))
      if (existing.isEmpty) {
        DicomSeries.makeDicomSeries(userPK, inputPK, machinePK, Seq(rtplan)) match {
          case Some(dicomSeries) => dicomSeries.insert
          case _ => logger.warn("Unable to create RTPLAN DicomSeries")
        }
      }
    }

    // handle RTPLANS differently than other series
    val rtplanAndOther = alList.partition(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTPLAN"))
    val rtplanList = rtplanAndOther._1
    // list of non-RTPLAN series
    val seriesList = rtplanAndOther._2.groupBy(al => Util.serInstOfAl(al)).map(s => s._2)

    rtplanList.map(rtplan => insertRtplanIfNew(rtplan))

    val insertedList = seriesList.map(series => DicomSeries.makeDicomSeries(userPK, inputPK, machinePK, series)).flatten.map(series => series.insert)
    logger.info("Number of non-RTPLAN DicomSeries inserted: " + insertedList.size)
  }

  /**
   * Run the analysis safely, catching any exceptions.  Enforce a timeout, and save the results.
   * The results include the termination status, finish date, and generated files.
   *
   * Strictly speaking, the timeout is not enforced.  If the analysis is running in a separate
   * thread, there is no way to kill it other than restarting the service.
   *
   * Restarting the service might be ok if logic was put in to wait until no analysis was being done.  TODO
   */
  private def runAnalysis(runTrait: RunTrait[RunReqClass], runReq: RunReqClass, extendedData: ExtendedData) = {
    try {
      val timeout = Duration(extendedData.procedure.timeoutInMs, TimeUnit.MILLISECONDS)
      val future = Future[ProcedureStatus.Value] {
        try {
          runTrait.run(extendedData, runReq)
        } catch {
          case t: Throwable => {
            ProcedureStatus.crash
          }
        }
      }
      val status = Await.result(future, timeout)
      saveResults(status, extendedData)
    } catch {
      case timeout: TimeoutException => {
        // TODO should kill the running procedure, but there is no good way to do that.
        saveResults(ProcedureStatus.timeout, extendedData)
      }
    }
  }

  /**
   * Save the process results to the database.
   */
  private def saveResults(newStatus: ProcedureStatus.Value, extendedData: ExtendedData) = {
    // write the status to a little file in the output directory
    ProcedureStatus.writeProcedureStatus(extendedData.output.dir, newStatus)

    // save the finish status and finish date to the Output in the database
    extendedData.output.updateStatusAndFinishDate(newStatus.toString, new Date)

    // zip up the conents of the Output directory and save them
    val zippedContent = extendedData.output.makeZipOfFiles
    extendedData.output.updateData(zippedContent)
  }

  /**
   * Create input + output and start the analysis.
   */
  private def process(valueMap: ValueMapT, request: Request, response: Response, runTrait: RunTrait[RunReqClass], runReq: RunReqClass, alList: Seq[AttributeList]): Unit = {
    val now = new Timestamp((new Date).getTime)
    val PatientID = runTrait.getPatientID(valueMap, alList)
    val machine: Machine = {
      runTrait.getMachine(valueMap, alList) match {
        case Some(mach) => mach
        case _ => getMachineFromSelector.get
      }
    }
    val user = getUser(valueMap)
    val dataDate = runTrait.getDataDate(valueMap, alList)

    val userPK = if (user.isDefined) user.get.userPK else None

    // create DB Input
    val inputWithoutDir = (new Input(None, None, now, userPK, machine.machinePK, PatientID, dataDate)).insert
    if (userPK.isDefined)
      saveDicomSeries(userPK.get, inputWithoutDir.inputPK, machine.machinePK, alList)

    // The input PK is needed to make the input directory, which creates a circular definition when making an
    // input row, but this is part of the compromise of creating a file hierarchy that has a consistent (as
    // practical) link to the database.
    val inputDir = makeInputDir(machine, runTrait.getProcedure, inputWithoutDir.inputPK.get)

    // move input files to their final resting place
    val oldDir = sessionDir(valueMap)
    if (oldDir.isDefined)
      renameFileTryingPersistently(oldDir.get, inputDir)
    else
      inputDir.mkdirs
    if (!inputDir.exists)
      throw new RuntimeException("Unable to rename temporary directory " + oldDir + " to input directory " + inputDir.getAbsolutePath)

    inputWithoutDir.updateDirectory(inputDir)
    val input = Input.get(inputWithoutDir.inputPK.get).get // update the directory
    input.putFilesInDatabaseFuture(inputDir)

    val outputDir = makeOutputDir(inputDir, now)
    //outputDir.mkdirs // create output directory

    val output = {
      val tempOutput = new Output(
        outputPK = None,
        inputPK = input.inputPK.get,
        directory = WebServer.fileToResultsPath(outputDir),
        procedurePK = runTrait.getProcedure.procedurePK.get,
        userPK,
        now,
        finishDate = None,
        dataDate = dataDate,
        analysisDate = Some(now),
        machinePK = machine.machinePK,
        status = ProcedureStatus.running.toString,
        dataValidity = DataValidity.valid.toString)
      val out = tempOutput.insert
      out
    }

    val extendedData = ExtendedData.get(output)

    // If this is the same data being re-submitted, then remove the old version of the analysis.  The
    // usual reasons are that the analysis was changed or the analysis aborted.
    Future {
      val redundantList = Output.redundantWith(output)
      redundantList.map(o => removeOutput(o))
      redundantList.map(o => removeInput(o.inputPK))
    }

    if (WebUtil.isAwait(valueMap)) {
      // wait for analysis to finish
      runAnalysis(runTrait, runReq, extendedData)
    } else {
      // run in a thread instead of a Future because the debugger in the Eclipse IDE does not handle Future's nicely.
      class RunIt extends Runnable {
        override def run = runAnalysis(runTrait, runReq, extendedData)
      }
      (new Thread(new RunIt)).start
    }
    ViewOutput.redirectToViewRunProgress(response, WebUtil.isAutoUpload(valueMap), output.outputPK.get)
  }

  /**
   * Respond to the 'Run' button.
   */
  private def runIfDataValid(valueMap: ValueMapT, request: Request, response: Response, runTrait: RunTrait[RunReqClass]) = {

    logger.info("Validating data")
    val form = makeForm(runTrait)
    val dicomFileList = dicomFilesInSession(valueMap)
    val alList = dicomFileList.map(df => df.attributeList).flatten

    runTrait.validate(valueMap, form, request, response, alList) match {
      case Left(errMap) => {
        logger.info("Bad request: " + errMap.keys.map(k => k + " : " + valueMap.get(k)).mkString("\n    "))
        form.setFormResponse(valueMap, errMap, runTrait.getProcedure.fullName, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReq) => {
        logger.info("Validated data for " + runTrait.getProcedure.fullName)
        if (isAwait(valueMap)) awaitTag.synchronized {
          process(valueMap, request, response, runTrait, runReq, alList)
        }
        else process(valueMap, request, response, runTrait, runReq, alList)
      }
    }
  }

  /**
   * Determine if user is authorized to perform redo.  To be authorized, the user must be from the
   * same institution as the original user.
   *
   * Being whitelisted is not sufficient, because things just get weird in terms of viewing and
   * ownership of the data.
   */
  private def userAuthorizedToModify(request: Request, response: Response, input: Input): Boolean = {
    val user = CachedUser.get(request).get

    val mach = Machine.get(input.machinePK.get).get
    val dataInstitution = mach.institutionPK
    val requestorsInstitution = user.institutionPK
    val same = dataInstitution == requestorsInstitution
    logger.info("user requesting redo.  Authorized: " + same)
    same
  }

  /**
   * Re-process the given data.  Input data is not touched, old Output is deleted, along with
   * its file tree and the data that references it.
   */
  private def redo(valueMap: ValueMapT, response: Response, runTrait: RunTrait[RunReqClass]) = {
    val request = response.getRequest
    val oldOutput = {
      val outputPK = valueMap(OutputList.redoTag).toLong
      Output.get(outputPK)
    }

    val input = {
      if (oldOutput.isDefined)
        Input.get(oldOutput.get.inputPK)
      else
        None
    }

    if (input.isDefined) {
      if (userAuthorizedToModify(request, response, input.get)) {
        val now = new Timestamp((new Date).getTime)
        val user = CachedUser.get(request)

        val newOutput = {
          val tempOutput = new Output(
            outputPK = None,
            inputPK = oldOutput.get.inputPK,
            directory = WebServer.fileToResultsPath(makeOutputDir(input.get.dir, now)),
            procedurePK = runTrait.getProcedure.procedurePK.get,
            user.get.userPK,
            now,
            finishDate = None,
            dataDate = input.get.dataDate,
            analysisDate = Some(now),
            machinePK = oldOutput.get.machinePK,
            status = ProcedureStatus.running.toString,
            dataValidity = DataValidity.valid.toString)
          val out = tempOutput.insert
          out
        }
        // now that new Output has been created, remove any old redundant inputs and outputs.
        Future {
          val redundantList = Output.redundantWith(newOutput)
          redundantList.map(o => removeOutput(o))
        }

        // instantiate the input files from originals
        val extendedData = ExtendedData.get(newOutput)
        val inputDir = extendedData.input.dir
        Util.deleteFileTreeSafely(inputDir)
        Input.getFilesFromDatabase(extendedData.input.inputPK.get, inputDir.getParentFile)

        // read the DICOM files
        val alList = Util.listDirFiles(inputDir).map(f => new DicomFile(f)).map(df => df.attributeList).flatten
        val runReq = runTrait.makeRunReq(alList)

        class RunIt extends Runnable {
          override def run = runAnalysis(runTrait, runReq, extendedData)
        }
        (new Thread(new RunIt)).start

        ViewOutput.redirectToViewRunProgress(response, WebUtil.isAutoUpload(valueMap), newOutput.outputPK.get)

      } else {
        "not authorized" // TODO
      }

    } else
      "no such output.  Has probably been deleted or redone." // TODO
  }

  private def emptyForm(valueMap: ValueMapT, response: Response, runTrait: RunTrait[RunReqClass]): Unit = {
    makeForm(runTrait).setFormResponse(valueMap, styleNone, runTrait.getProcedure.fullName, response, Status.SUCCESS_OK)
  }

  private def buttonIs(valueMap: ValueMapT, buttonName: String): Boolean = {
    val value = valueMap.get(buttonName)
    value.isDefined && value.get.toString.equals(buttonName)
  }

  /**
   * User elected to cancel.  Remove uploaded files.
   */
  private def cancel(valueMap: ValueMapT, response: Response) = {
    sessionDir(valueMap) match {
      case Some(dir) => Utility.deleteFileTree(dir)
      case _ => ;
    }
    WebRunIndex.redirect(response)
  }

  def handle(valueMap: ValueMapT, request: Request, response: Response, runTrait: RunTrait[RunReqClass]): Unit = {

    val redoValue = valueMap.get(OutputList.redoTag)
    val delValue = valueMap.get(OutputList.deleteTag)

    try {
      0 match {
        //case _ if (!sessionDefined(valueMap)) => redirectWithNewSession(response);
        case _ if buttonIs(valueMap, cancelButtonName) => cancel(valueMap, response)
        case _ if redoValue.isDefined => redo(valueMap, response, runTrait)
        case _ if buttonIs(valueMap, runButtonName) => runIfDataValid(valueMap, request, response, runTrait)
        case _ => emptyForm(valueMap, response, runTrait)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }

  }

}