package org.aqac.webrun

import org.restlet.Request
import org.restlet.Response
import slick.driver.PostgresDriver.api._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Status
import org.aqac.web.WebUtil._
import org.aqac.Logging._
import org.aqac.db.Machine
import edu.umro.ScalaUtil.Trace._
import java.io.File
import org.aqac.db.Procedure
import org.aqac.run.Run
import org.aqac.Util
import org.aqac.web.WebUtil
import org.aqac.db.MaxLeafGap

object MaxLeafGap_1 {
    val parametersFileName = "parameters.xml"
    val MaxLeafGap_1PKTag = "MaxLeafGap_1PK"
}

class MaxLeafGap_1(procedure: Procedure) extends WebRun(procedure) {

    /** Maximum tongue and groove offset in mm.  Exceeding this value probably indicates a user error. */
    private val maxTongueAndGrooveOffset = 10.0

    private val pageTitle = "Maximum Leaf Gap"

    def machineList() = Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

    private val machine = new WebInputSelect("Machine", 6, 0, machineList)

    private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
        val action = procedure.webUrl + "?" + name + "=" + name
        new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
    }

    private val runButton = makeButton("Run", true, ButtonType.BtnPrimary)
    private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

    private val form = new WebForm(procedure.webUrl, List(List(machine), List(runButton, cancelButton)), 6)

    private def emptyForm(response: Response) = {
        form.setFormResponse(emptyValueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
    }

    private def validate(valueMap: ValueMapT): StyleMapT = {
        val dir = sessionDir(valueMap)
        0 match {
            case _ if (!dir.isDirectory) => Error.make(form.uploadFileInput.get, "No files have been uploaded (no directory)") // TODO
            case _ if (dir.list.size < 2) => Error.make(form.uploadFileInput.get, "At least two files are required.") // TODO
            case _ => styleNone
        }
    }

    private val maxHistory = 30
    private val historyFileName = "history.txt"
    
    private def writeHistory(machinePK: Long, dir: File) = {
        val text = MaxLeafGap.getHistory(machinePK, maxHistory).map(g => g.toString + System.lineSeparator).foldLeft("")((t, gt) => t + gt)
        Util.writeFile(new File(dir, historyFileName), text)
    }

    /**
     * Run the procedure.
     */
    private def run(valueMap: ValueMapT, request: Request, response: Response) = {
        val errMap = validate(valueMap)
        if (errMap.isEmpty) {
            val machinePK = machine.getValOrEmpty(valueMap).toLong
            val dir = sessionDir(valueMap)
            writeHistory(machinePK, dir)
            val dtp = Util.dateTimeAndPatientIdFromDicom(dir)
            Run.run(procedure, Machine.get(machinePK).get, dir, request, response, dtp.PatientID, dtp.dateTime)
        }
        else
            form.setFormResponse(valueMap, errMap, pageTitle, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }

    private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
        val value = valueMap.get(button.label)
        value.isDefined && value.get.toString.equals(button.label)
    }

    override def handle(request: Request, response: Response): Unit = {

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
                internalFailure(response, "Unexpected failure: " + t.toString)
            }
        }
    }

}
