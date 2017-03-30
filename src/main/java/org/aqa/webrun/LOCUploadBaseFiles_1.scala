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
import edu.umro.util.Utility

object LOCUploadBaseFiles_1 {
    val parametersFileName = "parameters.xml"
    val LOCUploadBaseFiles_1PKTag = "LOCUploadBaseFiles_1PK"
}

/**
 * Runs procedures that only need the user to upload files and choose a treatment machine.
 */
class LOCUploadBaseFiles_1(procedure: Procedure) extends WebRunProcedure(procedure) {

    def machineList() = ("-1", "None") +: Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

    private val machine = new WebInputSelectOption("Machine", 6, 0, machineList, machineSpecRequired)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = procedure.webUrl + "?" + name + "=" + name
        new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private def form = new WebForm(procedure.webUrl, List(List(machine), List(runButton, cancelButton)), 6)

    private def emptyForm(valueMap: ValueMapT, response: Response) = {
        form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
    }

    private def needToChooseMachine(valueMap: ValueMapT): Boolean = {
        if (machinesInSession(valueMap).isEmpty) {
            Util.stringToLong(machine.getValOrEmpty(valueMap)) match {
                case Some(machinePK) if (machinePK < 0) => true
                case Some(machinePK) => Machine.get(machinePK).isEmpty
                case _ => true
            }
        }
        else true
    }

    private def validate(valueMap: ValueMapT): StyleMapT = {
        sessionDir(valueMap) match {
            case Some(dir) if (!dir.isDirectory) => Error.make(form.uploadFileInput.get, "No files have been uploaded")
            case Some(dir) if (dir.list.size != 2) => Error.make(form.uploadFileInput.get, "Exactly two files are required.")
            case _ if needToChooseMachine(valueMap) => Error.make(machine, "Machine needs to be chosen.")
            case _ => styleNone // No error
        }
    }

    //private val maxHistory = -1

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
            val mach: Machine = {
                val userMach = Machine.get(machine.getValOrEmpty(valueMap).toLong)
                if (userMach.isDefined) userMach.get
                else machinesInSession(valueMap).head
            }

            sessionDir(valueMap) match {
                case Some(dir) => {
                    val dtp = Util.dateTimeAndPatientIdFromDicom(dir)
                    establishSerialNumberAndMachConfig(dir, mach)
                    Run.run(procedure, Machine.get(mach.machinePK.get).get, dir, request, response, dtp.PatientID, dtp.dateTime)
                }
                case _ => throw new RuntimeException("Unexpected internal error. None in LOCUploadBaseFiles_1.run")
            }
        }
        else
            form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }

    /**
     * Cancel the procedure.  Remove files and redirect to procedure list.
     */
    private def cancel(valueMap: ValueMapT, response: Response) = {
        sessionDir(valueMap) match {
            case Some(dir) => Utility.deleteFileTree(dir)
            case _ => ;
        }
        response.redirectSeeOther("/")
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
        }
        catch {
            case t: Throwable => {
                internalFailure(response, "Unexpected failure: " + fmtEx(t))
            }
        }
    }

}
