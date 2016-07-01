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
    val path = WebUtil.pathOf(OutputList.getClass.getName)

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

    private def startTime(output: Output): String = startTimeFormat.format(output.startDate)

    private def summaryFileUrl(output: Output): Elem = {
        val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + output.outputPK.get + "&" + ViewOutput.summaryTag + "=true"
        <td><a href={ url }>Summary</a></td>
    }

    private val institutionCol = new Column[Output]("Institution", institutionName)

    private val userCol = new Column[Output]("User", user)

    private val startTimeCol = new Column[Output]("Start Time", startTime)

    private val summaryFileCol = new Column[Output]("Summary", _.directory, summaryFileUrl)

    private val statusCol = new Column[Output]("Status", _.status)

    private val procedureCol = new Column[Output]("Procedure", procedureName)

    private val machineCol = new Column[Output]("Machine", machineName)

    val colList = Seq(startTimeCol, summaryFileCol, procedureCol, machineCol, institutionCol, userCol, statusCol)
}

class OutputList extends GenericList[Output]("Output", OutputList.colList, "/ViewOutput") {

    override def getData = Output.list

    override def getPK(value: Output): Long = value.outputPK.get
}