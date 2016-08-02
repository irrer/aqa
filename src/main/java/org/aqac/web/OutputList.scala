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

object OutputList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.view, OutputList.getClass.getName)

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
        val input = Input.get(extendedValues.output.inputPK).get
        println("===== inputFileUrl: " + WebServer.urlOfDataPath(input.directory.get)) // TODO rm
        <td><a href={ WebServer.urlOfDataPath(input.directory.get) }>Input</a></td>
    }

    private val institutionCol = new Column[Output.ExtendedValues]("Institution", _.institution.name)

    private val userCol = new Column[Output.ExtendedValues]("User", _.user.id)

    private val startTimeCol = new Column[Output.ExtendedValues]("Start Time", startTime, startTimeUrl)

    private val summaryFileCol = new Column[Output.ExtendedValues]("Summary", _.output.directory, summaryFileUrl)

    private val inputFileCol = new Column[Output.ExtendedValues]("Input", _ => "Input", inputFileUrl)

    private val statusCol = new Column[Output.ExtendedValues]("Status", _.output.status)

    private val procedureCol = new Column[Output.ExtendedValues]("Procedure", _.procedure.name)

    private val machineCol = new Column[Output.ExtendedValues]("Machine", _.machine.id)

    val colList = Seq(startTimeCol, summaryFileCol, inputFileCol, procedureCol, machineCol, institutionCol, userCol, statusCol)
}

class OutputList extends GenericList[Output.ExtendedValues]("Output", OutputList.colList) with WebUtil.SubUrlView {

    val entriesPerPage = 30
    
    override def getData = Output.extendedList(None, None, entriesPerPage)

    override def getPK(extendedValues: Output.ExtendedValues): Long = extendedValues.output.outputPK.get

    override val canCreate: Boolean = false
}