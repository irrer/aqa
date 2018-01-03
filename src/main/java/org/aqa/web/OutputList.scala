package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import java.text.SimpleDateFormat
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.Institution
import java.io.File
import org.aqa.db.DataValidity
import org.restlet.Request
import org.aqa.web.WebUtil._
import org.aqa.db.CentralAxis
import edu.umro.util.Utility

object OutputList {
  val deleteTag = "delete"

  private val path = new String((new OutputList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class OutputList extends GenericList[Output.ExtendedValues] with WebUtil.SubUrlView {

  override def listName = "Output"

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
    } catch {
      case e: Exception => "none"
    }
  }

  private def institutionName(output: Output): String = {
    try {
      val input = Input.get(output.outputPK.get)
      val machine = Machine.get(input.get.machinePK.get)
      val institution = Institution.get(machine.get.institutionPK)
      institution.get.name
    } catch {
      case e: Exception => "none"
    }
  }

  private def user(output: Output): String = User.get(output.userPK.get).get.fullName

  private val startTimeFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss")

  private def compareByInputTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Int = {
    if (a.input.dataDate.isDefined && b.input.dataDate.isDefined) a.input.dataDate.get.compareTo(b.input.dataDate.get)
    else 0
  }

  private def compareByStartTime(a: Output.ExtendedValues, b: Output.ExtendedValues): Boolean = a.output.startDate.compareTo(b.output.startDate) > 0

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
    <a title="Data analysis time" href={ getUrl(extendedValues.output.outputPK.get, false) }> { startTimeFormat.format(extendedValues.output.startDate) }</a>
  }

  private def inputFileUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Data aquisition time" href={ WebServer.urlOfResultsPath(extendedValues.input.directory.get) }>{ inputTime(extendedValues) }</a>
  }

  private def invalidateRowName(extendedValues: Output.ExtendedValues): String = extendedValues.output.dataValidity.toString

  private def deleteUrl(extendedValues: Output.ExtendedValues): Elem = {
    <a title="Click to delete.  Can NOT be undone" href={ OutputList.path + "?" + OutputList.deleteTag + "=" + extendedValues.output.outputPK.get }>Delete</a>
  }

  type ColT = Output.ExtendedValues

  private val institutionCol = new Column[ColT]("Institution", _.institution.name)

  private val userCol = new Column[ColT]("User", _.user.id)

  private val startTimeCol = new Column[ColT]("Analysis", compareByStartTime _, startTimeUrl _)

  private val inputFileCol = new Column[ColT]("Acquisition", inputTime _, inputFileUrl _)

  private val deleteCol = new Column[ColT]("Delete", _ => "Delete", deleteUrl)

  private val procedureCol = new Column[ColT]("Procedure", (d) => d.procedure.name + " " + d.procedure.version)

  private val machineCol = new Column[ColT]("Machine", _.machine.id)

  override val columnList = Seq(startTimeCol, inputFileCol, procedureCol, machineCol, institutionCol, userCol, deleteCol)

  val entriesPerPage = 1000

  override def getData(valueMap: ValueMapT) = Output.extendedList(None, None, entriesPerPage)

  override def getPK(extendedValues: Output.ExtendedValues): Long = extendedValues.output.outputPK.get

  override val canCreate: Boolean = false

  /** Remove the output, the other associated outputs, and the directory. If its input is only referenced by this output, then delete the input too. */
  private def deleteOutput(outputPK: Long): Unit = {
    val output = Output.get(outputPK)
    if (output.isDefined) {
      Output.delete(output.get.outputPK.get)
      val list = Output.listByInputPK(output.get.inputPK)
      if (list.size == 0) {
        val input = Input.get(output.get.inputPK)
        Input.delete(input.get.inputPK.get)
        Utility.deleteFileTree(input.get.dir)
      } else Utility.deleteFileTree(output.get.dir)
    }
  }

  override def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Unit = {
    try {
      val delete = valueMap.get(OutputList.deleteTag)

      0 match {
        case _ if (delete.isDefined) => deleteOutput(delete.get.toLong)
        case _ => ;
      }
    } catch {
      case t: Throwable => internalFailure(response, "Unexpected error in OutputList: " + fmtEx(t))
    }
  }
}
