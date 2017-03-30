package org.aqa.webrun

import org.restlet.Request
import org.restlet.Response
import slick.driver.PostgresDriver.api._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging._
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

object UploadAndChooseMachine_1 {
    val parametersFileName = "parameters.xml"
    val UploadAndChooseMachine_1PKTag = "UploadAndChooseMachine_1PK"
}

/**
 * Runs procedures that only need the user to upload files and choose a treatment machine.
 */
class UploadAndChooseMachine_1(procedure: Procedure) extends WebRunProcedure(procedure) {

    /** Maximum tongue and groove offset in mm.  Exceeding this value probably indicates a user error. */
    private val maxTongueAndGrooveOffset = 10.0

    def machineList() = Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

    private val machine = new WebInputSelect("Machine", 6, 0, machineList)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = procedure.webUrl + "?" + name + "=" + name
        new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(procedure.webUrl, List(List(machine), List(runButton, cancelButton)), 6)

    private def emptyForm(response: Response) = {
        form.setFormResponse(emptyValueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
    }

    private def validate(valueMap: ValueMapT): StyleMapT = {
        lazy val noDir = Error.make(form.uploadFileInput.get, "No files have been uploaded (no directory)")
        sessionDir(valueMap) match {
            case None => noDir
            case Some(dir) if (!dir.isDirectory) => noDir
            case Some(dir) if (dir.list.isEmpty) => Error.make(form.uploadFileInput.get, "At least one file is required.")
            case _ => styleNone
        }
    }

    private val maxHistory = -1
    private val historyFileName = "history.txt"

    private def writeHistory(machinePK: Long, dir: File) = {
        val text = CentralAxis.getHistory(machinePK, maxHistory).map(g => g.toString + System.lineSeparator).foldLeft("")((t, gt) => t + gt)
        Util.writeFile(new File(dir, historyFileName), text)
    }

    /** Establish the serial number and machine configuration directory.  Return true if it has been updated. */
    private def establishSerialNumberAndMachConfig(inputDir: File, machine: Machine): Boolean = {
        if (List(machine.serialNumber, machine.configurationDirectory).flatten.isEmpty) { // if the serial number and machine configuration directory are not defined
            val alList = inputDir.listFiles.toList.filter(f => DicomFileUtilities.isDicomOrAcrNemaFile(f)).map(f => Util.readDicomFile(f)).filter(r => r.isRight).map(r => r.right.get)
            val dsnList = alList.map(al => Util.getAttrValue(al, TagFromName.DeviceSerialNumber)).flatten
            if (dsnList.nonEmpty) {
                Machine.setSerialNumber(machine.machinePK.get, dsnList.head)
                true
            }
            else false
        }
        else false
    }

    /**
     * Run the procedure.
     */
    private def run(valueMap: ValueMapT, request: Request, response: Response) = {
        val errMap = validate(valueMap)
        if (errMap.isEmpty) {
            val machinePK = machine.getValOrEmpty(valueMap).toLong
            sessionDir(valueMap) match {
                case Some(dir) => {
                    writeHistory(machinePK, dir)
                    val dtp = Util.dateTimeAndPatientIdFromDicom(dir)
                    establishSerialNumberAndMachConfig(dir, Machine.get(machinePK).get)
                    Run.run(procedure, Machine.get(machinePK).get, dir, request, response, dtp.PatientID, dtp.dateTime)
                }
                case _ => throw new RuntimeException("Unexpected internal error. None in UploadAndChooseMachine_1.run")
            }
        }
        else
            form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
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
                case _ if buttonIs(valueMap, cancelButton) => response.redirectSeeOther("/")
                case _ if buttonIs(valueMap, runButton) => run(valueMap, request, response)
                case _ => emptyForm(response)
            }
        }
        catch {
            case t: Throwable => {
                internalFailure(response, "Unexpected failure: " + fmtEx(t))
            }
        }
    }

}
