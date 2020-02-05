package org.aqa.webrun

import org.restlet.Request
import org.restlet.Response
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import edu.umro.ScalaUtil.Trace._
import java.io.File
import org.aqa.db.Procedure
import org.aqa.run.Run
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.db.CentralAxis
import org.restlet.Restlet
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.TagFromName
import edu.umro.util.Utility
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebRunIndex
import org.aqa.db.Machine.MMI
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.db.Input
import org.aqa.procedures.UploadTransAndOpen
import org.aqa.Config
import edu.umro.ScalaUtil.Trace

object LOCUploadBaseFiles_1 extends Logging {
  val parametersFileName = "parameters.xml"
  val LOCUploadBaseFiles_1PKTag = "LOCUploadBaseFiles_1PK"

  /**
   * Given a machine PK, make sure that the baseline files are available, getting them from the
   * database if necessary.  If it is not possible to get the files, then return false.
   */
  def ensureBaseline(machinePK: Long): Boolean = {

    try {
      val machine = Machine.get(machinePK).get

      def valid(dir: File): Boolean = {
        val ok = dir.isDirectory &&
          (new File(dir, UploadTransAndOpen.openName)).canRead &&
          (new File(dir, UploadTransAndOpen.transName)).canRead
        ok
      }

      val machConfValid: Boolean = {
        try {
          val v = valid(machine.configDir.get)
          v
        } catch {
          case t: Throwable => false
        }
      }

      /**
       * Copy the LOC baseline files from the output directory to the machine configuration directory.
       */
      def copyOutput(output: Output) = {
        val machConfigDir = machine.configDir.get
        machConfigDir.mkdirs
        def copy(name: String) = {
          val buf = Utility.readBinFile(new File(output.dir, name))
          Utility.writeFile(new File(machConfigDir, name), buf)
        }

        copy(UploadTransAndOpen.openName)
        copy(UploadTransAndOpen.transName)
        true
      }

      if (machConfValid) true
      else {
        logger.info("LOC machine configuration not available for " + machine.id)
        Output.getLatestLOCBaselineDir(machinePK, "LOCUploadBaseFiles_1") match {
          // The baseline files exist in the output directory.  Copy them to the machine configuration directory
          case Some((input, output)) if (valid(output.dir)) => copyOutput(output)

          // The base files exist only in the database.  Re-instate the input and output directories from the
          // database and then copy them to machine configuration directory
          case Some((input, output)) => {
            Input.getFilesFromDatabase(input.inputPK.get, input.dir.getParentFile)
            Output.getFilesFromDatabase(output.outputPK.get, output.dir.getParentFile)
            copyOutput(output)
          }
          case _ => false
        }
      }

    } catch {
      // if anything goes wrong, then the files are not available
      case t: Throwable => false
    }
  }

}

/**
 * Runs procedures that only need the user to upload files and choose a treatment machine.
 */
class LOCUploadBaseFiles_1(procedure: Procedure) extends WebRunProcedure(procedure) with Logging {

  private def getInstructions(valueMap: ValueMapT): Elem = {
    <div>Drag and drop the two DICOM baseline files for open  and trans.</div>
  }

  private val instructions = new WebPlainText("Instructions", false, 6, 0, getInstructions _)

  private val machine = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("Upload LOC Baseline"), List(List(instructions), List(machine), List(runButton, cancelButton)), 6)

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private case class RunRequirements(machine: Machine, serialNumber: Option[String], sessionDir: File, alList: Seq[AttributeList]);

  private def validate(valueMap: ValueMapT): Either[StyleMapT, RunRequirements] = {
    val alList = attributeListsInSession(valueMap)

    val serNoList = alList.map(al => Util.attributeListToDeviceSerialNumber(al)).flatten.distinct

    // machines that DICOM files reference (based on device serial numbers)
    val machList = alList.map(al => Machine.attributeListToMachine(al)).flatten.distinct

    // machine that user chose
    val chosenMach = for (pkTxt <- valueMap.get(machine.label); pk <- Util.stringToLong(pkTxt); mach <- Machine.get(pk)) yield mach

    // The machine to use
    val mach = List(chosenMach, machList.headOption).flatten.headOption

    def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

    sessionDir(valueMap) match {
      case Some(dir) if (!dir.isDirectory) => formErr("No files have been uploaded")
      case _ if (alList.isEmpty) => formErr("No DICOM files have been uploaded.")
      case _ if (alList.size == 1) => formErr("Only one DICOM file has been loaded.  Two are required.")
      case _ if (alList.size > 2) => formErr("More than two DICOM files have been loaded.  Exactly two are required.  Click Cancel to start over.")
      case _ if (serNoList.size > 1) => formErr("Files from more than two different machines were found.  Click Cancel to start over.")
      case _ if (machList.isEmpty && chosenMach.isEmpty) => Left(Error.make(machine, "A machine must be chosen"))
      case _ if (mach.isEmpty) => Left(Error.make(machine, "A machine needs to be chosen"))
      case Some(dir) => {
        val newSerialNumber: Option[String] = chosenMach match {
          case Some(m) => serNoList.headOption
          case _ => None
        }
        logger.info("LOCUploadBaseFiles_1 newSerialNumber: " + newSerialNumber)
        Right(new RunRequirements(mach.get, newSerialNumber, dir, alList))
      }
    }
  }

  /**
   * Run the procedure.
   */
  private def run(valueMap: ValueMapT, request: Request, response: Response) = {
    validate(valueMap) match {
      case Right(runReq) => {
        val machPK = runReq.machine.machinePK.get
        // set the machine's serial number (and config dir) if necessary
        logger.info("runReq.serialNumber: " + runReq.serialNumber)
        logger.info("runReq.machine.serialNumber: " + runReq.machine.serialNumber)
        (runReq.serialNumber, runReq.machine.serialNumber) match {
          case (Some(newSer), Some(oldSer)) if (newSer != oldSer) => Machine.setSerialNumber(machPK, newSer)
          case (Some(newSer), _) => Machine.setSerialNumber(runReq.machine.machinePK.get, newSer)
          case _ => ;
        }

        val dtp = Util.dateTimeAndPatientIdFromDicom(runReq.sessionDir)

        Run.run(procedure, Machine.get(machPK).get, runReq.sessionDir, request, response, dtp.PatientID, dtp.dateTime, None)
      }
      case Left(errMap) => form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
   * Cancel the procedure.  Remove files and redirect to procedure list.
   */
  private def cancel(valueMap: ValueMapT, response: Response) = {
    sessionDir(valueMap) match {
      case Some(dir) => {
        logger.info("Removing directory tree " + dir.getAbsolutePath)
        Util.deleteFileTreeSafely(dir)
      }
      case _ => ;
    }
    WebRunIndex.redirect(response)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)

    try {
      0 match {
        case _ if buttonIs(valueMap, cancelButton) => cancel(valueMap, response)
        case _ if buttonIs(valueMap, runButton) => run(valueMap, request, response)
        case _ => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
