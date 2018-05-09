package org.aqa.webrun.phase2

import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import java.io.File
import org.aqa.db.Procedure
import org.aqa.Util
import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import edu.umro.util.Utility
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebRunIndex
import org.aqa.run.PostProcess
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutputUtil
import scala.xml.XML
import org.aqa.db.PositioningCheck
import com.pixelmed.dicom.SOPClass
import org.aqa.db.Input
import java.sql.Timestamp
import java.util.Date
import org.aqa.run.Run
import org.aqa.run.ProcedureStatus
import org.aqa.db.Output
import org.aqa.db.Institution
import org.aqa.db.User
import org.aqa.web.ViewOutput
import org.aqa.web.DicomAccess
import org.aqa.web.WebServer
import com.pixelmed.dicom.TimeAttribute
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.WebRunProcedure
import org.aqa.webrun.LOCSpreadsheet
import org.aqa.webrun.LOCXml
import org.aqa.db.Machine
import org.aqa.webrun.RunRequirements

object Phase2 {
  val parametersFileName = "parameters.xml"
  val Phase2RunPKTag = "Phase2RunPK"
}

/**
 * Run Phase2 code.
 */
class Phase2(procedure: Procedure) extends WebRunProcedure(procedure) with Logging {

  /** Defines precision - Format to use when showing numbers. */
  private val outputFormat = "%7.5e"

  private val machineSelector = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("Phase2"), List(List(machineSelector), List(runButton, cancelButton)), 6)

  private def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private def validateMachineSelection(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, Machine] = {
    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
    // machines that DICOM files reference (based on device serial numbers)
    val referencedMachines = rtimageList.map(df => attributeListToMachine(df.attributeList.get)).flatten.distinct
    val chosenMachine = for (pkTxt <- valueMap.get(machineSelector.label); pk <- Util.stringToLong(pkTxt); m <- Machine.get(pk)) yield m

    val result: Either[StyleMapT, Machine] = 0 match {
      case _ if (referencedMachines.size == 1) => Right(referencedMachines.head)
      case _ if (chosenMachine.isDefined) => Right(chosenMachine.get)
      case _ if (referencedMachines.size > 1) => formErr("Files come from more than one machine; please go back and try again.  Machines: " + referencedMachines.map(m => m.id).mkString("  "))
      case _ => formErr("Unknown machine.  Please use the Administration interface to add it.")
    }
    result
  }

  case class RunReq(positioningCheck: PositioningCheckRunRequirements) {
    def reDir(dir: File): RunReq = new RunReq(positioningCheck.reDir(dir))
  }

  /**
   * Perform some general preliminary checks on the input files.
   */
  private def basicFileValidation(sessDir: Option[File], dicomFileList: Seq[DicomFile], rtimageList: Seq[DicomFile]) = {
    sessDir match {
      case Some(sd) if (!sd.isDirectory) => Error.make(form.uploadFileInput.get, "No files uploaded.")
      case Some(sd) if (Util.listDirFiles(sd).isEmpty) => Error.make(form.uploadFileInput.get, "No files have been uploaded.")
      case _ if (dicomFileList.isEmpty) => Error.make(form.uploadFileInput.get, "No DICOM files have been uploaded.")
      case _ if (rtimageList.isEmpty) => Error.make(form.uploadFileInput.get, "No DICOM RTIMAGE files have been uploaded.")
      case _ => styleNone
    }
  }

  private def validate(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, RunReq] = {
    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))

    val sessDir = sessionDir(valueMap)

    basicFileValidation(sessDir, dicomFileList, rtimageList) match {
      case err if (err.nonEmpty) => Left(err)
      case _ => {

      }
    }

    val result = validateMachineSelection(valueMap, rtimageList) match {
      case Left(err) => Left(err)
      case Right(machine) => {
        val positioningCheck = PositioningCheckValidation.validate(valueMap, None, machine, form.uploadFileInput)
        positioningCheck match {
          case Left(err) => Left(err)
          case Right(chkAnglRR) => Right(new RunReq(chkAnglRR))
        }
      }
    }
    result
  }

  private def dateTimePatId(rtimageList: Seq[DicomFile]) = {
    val list = rtimageList.map(df => Util.dateTimeAndPatientIdFromDicom(df.file))
    if (list.isEmpty) new Util.DateTimeAndPatientId(None, None)
    else list.minBy(dt => dt.dateTime)
  }

  private def makeHtml(output: Output, procedureStatus: ProcedureStatus.Value, elemList: Seq[Elem]) = {

    def table = {
      <table>
        <tr>
          { elemList.map(e => <td>{ e }</td>) }
        </tr>
      </table>
    }

    val text = Phase2Util.wrapSubProcedure(output, table, "Phase 2", procedureStatus)
    val file = new File(output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)
  }

  /**
   * Run the sub-procedures.
   */
  private def runPhase2(output: Output, runReq: RunReq): ProcedureStatus.Value = {
    val summary = PositioningCheckAnalysis.runProcedure(output, runReq.positioningCheck)
    val iiElem = summary._2
    makeHtml(output, summary._1, Seq(summary._2))
    summary._1
  }

  /**
   * Respond to the 'Run' button.
   */
  private def run(valueMap: ValueMapT, request: Request, response: Response) = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
    validate(valueMap, dicomFileList) match {
      case Left(errMap) => {
        form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReqSession) => {
        // only consider the RTIMAGE files for the date-time stamp.  The plan could have been from months ago.
        val dtp = dateTimePatId(rtimageList)

        val sessDir = sessionDir(valueMap).get
        val inputOutput = Run.preRun(procedure, runReqSession.positioningCheck.machine, sessDir, getUser(request), dtp.PatientID, dtp.dateTime)
        val input = inputOutput._1
        val output = inputOutput._2

        val runReqFinal = runReqSession.reDir(input.dir)

        val plan = runReqFinal.positioningCheck.plan
        val machine = runReqFinal.positioningCheck.machine
        Phase2Util.saveRtplan(plan)

        val finalStatus = runPhase2(output, runReqFinal)
        val finDate = new Timestamp(System.currentTimeMillis)
        val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

        Phase2Util.setMachineSerialNumber(machine, runReqFinal.positioningCheck.imageIdFileList.head.dicomFile.attributeList.get)
        outputFinal.insertOrUpdate
        outputFinal.updateData(outputFinal.makeZipOfFiles)
        Run.removeRedundantOutput(outputFinal.outputPK)

        val suffix = "?" + ViewOutput.outputPKTag + "=" + outputFinal.outputPK.get
        response.redirectSeeOther(ViewOutput.path + suffix)
      }
    }
  }

  /**
   * Cancel the procedure.  Remove files and redirect to procedure list.
   */
  private def cancel(valueMap: ValueMapT, response: Response) = {
    sessionDir(valueMap) match {
      case Some(dir) => Utility.deleteFileTree(dir)
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
        //case _ if (!sessionDefined(valueMap)) => redirectWithNewSession(response);
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
