package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.Output
import org.aqac.db.Procedure
import org.aqac.db.User
import java.text.SimpleDateFormat
import org.aqac.db.Input
import org.aqac.db.Machine
import org.aqac.db.Institution
import java.io.File
import org.aqac.db.DataValidity
import org.restlet.Request
import WebUtil._
import org.aqac.db.CentralAxis
import edu.umro.util.Utility

object OutputList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.view, OutputList.getClass.getName)

    val dataValidityTag = "dataValidity"
    val deleteTag = "delete"

    def redirect(response: Response) = response.redirectSeeOther(path)

    private def humanReadableURL(url: String): String = {
        val small = url.replaceAll("^https://", "").replaceAll("^http://", "").replaceAll("^www\\.", "")
        WebUtil.firstPartOf(small, 20)
    }

    private def procedureName(output: Output): String = Procedure.get(output.procedurePK).get.fullName

    private def machineName(output: Output): String = { // TODO probably a nice place to use a monad
        try {
            val input = Input.get(output.outputPK.get)
            val machine = Machine.get(input.get.machinePK.get)
            machine.get.id
        }
        catch {
            case e: Exception => "none"
        }
    }

    private def institutionName(output: Output): String = {
        try {
            val input = Input.get(output.outputPK.get)
            val machine = Machine.get(input.get.machinePK.get)
            val institution = Institution.get(machine.get.institutionPK)
            institution.get.name
        }
        catch {
            case e: Exception => "none"
        }
    }

    private def user(output: Output): String = User.get(output.userPK.get).get.fullName

    private val startTimeFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss")

    private def startTime(extendedValues: Output.ExtendedValues): String = startTimeFormat.format(extendedValues.output.startDate)

    private def inputTime(extendedValues: Output.ExtendedValues): String = {
        val date = extendedValues.input.dataDate
        if (date.isDefined) startTimeFormat.format(date.get) else "unknown date"
    }

    private def getUrl(outputPK: Long, summary: Boolean): String = {
        val sum = if (summary) ("&" + ViewOutput.summaryTag + "=true") else ""
        val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + outputPK + sum
        url
    }

    private def startTimeUrl(extendedValues: Output.ExtendedValues): Elem = {
        println("calling startTimeUrl") // TODO rm
        <td><a href={ getUrl(extendedValues.output.outputPK.get, false) }> { startTime(extendedValues) } </a></td>
    }

    private def summaryFileUrl(extendedValues: Output.ExtendedValues): Elem = {
        <td><a href={ getUrl(extendedValues.output.outputPK.get, true) }>Summary</a></td>
    }

    private def inputFileUrl(extendedValues: Output.ExtendedValues): Elem = {
        <td><a href={ WebServer.urlOfDataPath(extendedValues.input.directory.get) }>{ inputTime(extendedValues) }</a></td>
    }

    private def invalidateRowName(extendedValues: Output.ExtendedValues): String = extendedValues.output.dataValidity.toString

    private def invalidateUrl(extendedValues: Output.ExtendedValues): Elem = {
        <td><a title="Click to change.  Can be undone" href={ path + "?" + OutputList.dataValidityTag + "=" + extendedValues.output.outputPK.get }>{ extendedValues.output.dataValidity.toString }</a></td>
    }

    private def deleteUrl(extendedValues: Output.ExtendedValues): Elem = {
        <td><a title="Click to delete.  Can NOT be undone" href={ path + "?" + OutputList.deleteTag + "=" + extendedValues.output.outputPK.get }>Delete</a></td>
    }

    private val institutionCol = new Column[Output.ExtendedValues]("Institution", _.institution.name)

    private val userCol = new Column[Output.ExtendedValues]("User", _.user.id)

    private val startTimeCol = new Column[Output.ExtendedValues]("Start Time", startTime, startTimeUrl)

    private val summaryFileCol = new Column[Output.ExtendedValues]("Summary", _.output.directory, summaryFileUrl)

    private val inputFileCol = new Column[Output.ExtendedValues]("Input", inputTime, inputFileUrl)

    private val statusCol = new Column[Output.ExtendedValues]("Status", _.output.status)

    private val invalidateCol = new Column[Output.ExtendedValues]("Valid Status", invalidateRowName, invalidateUrl)

    private val deleteCol = new Column[Output.ExtendedValues]("Delete", _ => "Delete", deleteUrl)

    private val procedureCol = new Column[Output.ExtendedValues]("Procedure", _.procedure.name)

    private val machineCol = new Column[Output.ExtendedValues]("Machine", _.machine.id)

    val colList = Seq(startTimeCol, summaryFileCol, inputFileCol, procedureCol, machineCol, institutionCol, userCol, statusCol, invalidateCol, deleteCol)
}

class OutputList extends GenericList[Output.ExtendedValues]("Output", OutputList.colList) with WebUtil.SubUrlView {

    val entriesPerPage = 30

    override def getData = Output.extendedList(None, None, entriesPerPage)

    override def getPK(extendedValues: Output.ExtendedValues): Long = extendedValues.output.outputPK.get

    override val canCreate: Boolean = false

    /** Change the dataValidity column. */
    private def changeDataValidity(outputPK: Long): Unit = {

        val output = Output.get(outputPK).get
        val ovo = DataValidity.stringToDataValidity(output.dataValidity)
        val newValidity = {
            if (ovo.isDefined && (ovo.get == DataValidity.valid)) {
                DataValidity.invalid
            }
            else DataValidity.valid
        }

        val updatedOutput = new Output(
            outputPK = output.outputPK,
            inputPK = output.inputPK,
            directory = output.directory,
            procedurePK = output.procedurePK,
            userPK = output.userPK,
            startDate = output.startDate,
            finishDate = output.finishDate,
            status = output.status,
            dataValidity = newValidity.toString)

        updatedOutput.insertOrUpdate
    }

    /** Remove the output, the other associated outputs, and the directory. If its input is only referenced by this output, then delete the input too. */
    private def deleteOutput(outputPK: Long): Unit = {
        val output = Output.get(outputPK)
        if (output.isDefined) {
            Output.deleteOutputAndReferences(output.get.outputPK.get)
            val list = Output.listByInputPK(output.get.inputPK)
            if (list.size == 0) {
                val input = Input.get(output.get.inputPK)
                Input.delete(input.get.inputPK.get)
                Utility.deleteFileTree(input.get.dir)
            }
            else Utility.deleteFileTree(output.get.dir)
        }
    }

    override def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Unit = {
        try {
            val dataValidity = valueMap.get(OutputList.dataValidityTag)
            val delete = valueMap.get(OutputList.deleteTag)

            0 match {
                case _ if (dataValidity.isDefined) => { val dv = changeDataValidity(dataValidity.get.toLong) }
                case _ if (delete.isDefined) => deleteOutput(delete.get.toLong)
                case _ => ;
            }
        }
        catch {
            case t: Throwable => internalFailure(response, "Unexpected error in OutputList: " + t.toString)
        }
    }

}