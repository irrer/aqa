package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.PMI
import java.sql.Timestamp
import org.aqa.web.WebUtil._
import org.aqa.db.User
import org.aqa.db.Machine

object PMIList {
  val path = new String((new PMIList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class PMIList extends GenericList[PMI] with WebUtil.SubUrlAdmin {
  override def listName = "PMI"

  override def getPKName: String = "pmiPK"

  /**
   * If a machinePK is given, then filter on that, otherwise list maintenance records for all machines.
   */
  override def getData(valueMap: ValueMapT) = {
    valueMap.get(MachineUpdate.machinePKTag) match {
      case Some(machinePK) => PMI.getByMachine(machinePK.toLong)
      case _ => PMI.list
    }
  }

  override def getPK(value: PMI): Long = value.pmiPK.get

  private def machineParameter(valueMap: ValueMapT): String = { "?" + MachineUpdate.machinePKTag + "=" + valueMap(MachineUpdate.machinePKTag) }

  override def createNewPath(valueMap: ValueMapT): String = {
    WebUtil.cleanClassName(PMIUpdate.getClass.getName) + machineParameter(valueMap)
  }

  override def createNew(valueMap: ValueMapT): Elem = {
    val machinePath = WebUtil.cleanClassName(MachineUpdate.getClass.getName) + machineParameter(valueMap)
    val machineName: String = {
      try {
        Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get.id
      } catch {
        case t: Throwable => ""
      }
    }
    <div class="row col-md-2 col-md-offset-10">
      <strong>
        <a href={ createNewPath(valueMap) }>Create new { listName }</a><p> </p>
        <a href={ machinePath }>Return to machine { " " + machineName }</a><p> </p>
      </strong>
    </div>;
  }

  private def descHTML(pmi: PMI): Elem = <div>{ WebUtil.firstPartOf(pmi.description, 60) }</div>

  private val dateTimeCol = new Column[PMI](
    "Date/Time",
    (a, b) => (a.creationTime.getTime < b.creationTime.getTime), (mr: PMI) => makePrimaryKeyHtml(WebInputDateTime.dateTimeFormat.format(mr.creationTime), mr.pmiPK))

  private val userCol = new Column[PMI]("User", mr => User.get(mr.userPK).get.id, mr => wrapAlias(User.get(mr.userPK).get.id))

  private val summaryCol = new Column[PMI]("Summary", _.summary)

  private val descriptionCol = new Column[PMI]("Description", _.description, descHTML)

  override val columnList = Seq(dateTimeCol, userCol, summaryCol, descriptionCol)
}
