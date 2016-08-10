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
import org.aqac.db.MaxLeafGap
import edu.umro.util.Utility

object OutputList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.view, OutputList.getClass.getName)

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
        <td><a href={ WebServer.urlOfDataPath(extendedValues.input.directory.get) }>Input</a></td>
    }

    private def invalidateUrl(extendedValues: Output.ExtendedValues): Elem = {
        <td><a href={ path + "?" + DataValidity.invalid.toString + "=" + extendedValues.output.outputPK.get }>Invalidate</a></td>
    }

    private def deleteUrl(extendedValues: Output.ExtendedValues): Elem = {
        <td><a href={ path + "?" + OutputList.deleteTag + "=" + extendedValues.output.outputPK.get }>Delete</a></td>
    }

    private val institutionCol = new Column[Output.ExtendedValues]("Institution", _.institution.name)

    private val userCol = new Column[Output.ExtendedValues]("User", _.user.id)

    private val startTimeCol = new Column[Output.ExtendedValues]("Start Time", startTime, startTimeUrl)

    private val summaryFileCol = new Column[Output.ExtendedValues]("Summary", _.output.directory, summaryFileUrl)

    private val inputFileCol = new Column[Output.ExtendedValues]("Input", _ => "Input", inputFileUrl)

    private val statusCol = new Column[Output.ExtendedValues]("Status", _.output.status)

    private val invalidateCol = new Column[Output.ExtendedValues]("Invalidate", _ => "Invalidate", invalidateUrl)

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

    /** Set the dataValidity column to invalid. */
    private def invalidateOutput(outputPK: Long): Unit = {
        val output = Output.get(outputPK).get

        val updatedOutput = new Output(
            outputPK = output.outputPK,
            inputPK = output.inputPK,
            directory = output.directory,
            procedurePK = output.procedurePK,
            userPK = output.userPK,
            startDate = output.startDate,
            finishDate = output.finishDate,
            status = output.status,
            dataValidity = DataValidity.invalid.toString)

        updatedOutput.insertOrUpdate

    }

    /** Remove the output, the other associated outputs, and the directory. If its input is only referenced by this output, then delete the input too. */
    private def deleteOutput(outputPK: Long): Unit = {

        val output = Output.get(outputPK)
        if (output.isDefined) {
            Output.deleteOutputAndReferences(outputPK)
            val list = Output.listByInputPK(output.get.inputPK)
            println("list.size: " + list.size)   // TODO rm
            list.map(o => println(o))             // TODO rm
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
            val invalidate = valueMap.get(DataValidity.invalid.toString)
            val delete = valueMap.get(OutputList.deleteTag)

            0 match {
                case _ if (invalidate.isDefined) => invalidateOutput(invalidate.get.toLong)
                case _ if (delete.isDefined) => deleteOutput(delete.get.toLong)
                case _ => ;
            }
        }
        catch {
            case t: Throwable => internalFailure(response, "Unexpected error in OutputList: " + t.toString)
        }
    }

}