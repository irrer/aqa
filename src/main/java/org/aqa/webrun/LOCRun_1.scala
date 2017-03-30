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
import com.pixelmed.dicom.AttributeList

object LOCRun_1 {
    val parametersFileName = "parameters.xml"
    val LOCRun_1PKTag = "LOCRun_1PK"
}

/**
 * Run LOC code.
 */
class LOCRun_1(procedure: Procedure) extends WebRunProcedure(procedure) {

    def machineList() = ("-1", "None") +: Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = procedure.webUrl + "?" + name + "=" + name
        new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private def form = new WebForm(procedure.webUrl, List(List(runButton, cancelButton)), 6)

    private def emptyForm(valueMap: ValueMapT, response: Response) = {
        form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
    }

    private case class RunRequirements(machine: Machine, sessionDir: File, alList: Seq[AttributeList]);

    private def validate(valueMap: ValueMapT): Either[StyleMapT, RunRequirements] = {
        lazy val alList = attributeListsInSession(valueMap)

        // machines that DICOM files reference (based on device serial numbers)
        lazy val machList = alList.map(al => attributeListToMachine(al)).flatten.distinct

        // The machine to use
        lazy val mach = machList.headOption

        def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

        sessionDir(valueMap) match {
            case Some(dir) if (!dir.isDirectory) => formErr("No files have been uploaded")
            case _ if (alList.isEmpty) => formErr("No DICOM files have been uploaded.")
            case _ if (machList.size > 1) => formErr("Files from more than one machine were found.  Click Cancel to start over.")
            case _ if (machList.isEmpty) => formErr("These files do not have a serial number of a known machine.  Click Cancel and use the baseline LOC upload procedure.")
            case Some(dir) => Right(new RunRequirements(mach.get, dir, alList))
        }
    }

    /**
     * Run the procedure.
     */
    private def run(valueMap: ValueMapT, request: Request, response: Response) = {
        validate(valueMap) match {
            case Right(runReq) => {
                val machPK = runReq.machine.machinePK.get

                val dtp = Util.dateTimeAndPatientIdFromDicom(runReq.sessionDir)

                Run.run(procedure, Machine.get(machPK).get, runReq.sessionDir, request, response, dtp.PatientID, dtp.dateTime)
            }
            case Left(errMap) => form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
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
