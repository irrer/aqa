package org.aqa.web

import org.restlet.Response
import org.aqa.web.WebUtil._
import org.aqa.Config
import java.io.File
import org.aqa.db.SystemModification

object SystemModificationList {
  val path = new String((new SystemModificationList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)

}

class SystemModificationList extends GenericList[SystemModification.SystemModificationComposite] with WebUtil.SubUrlAdmin {

  private type SMC = SystemModification.SystemModificationComposite

  override def listName = "System Modification"

  private def compareByDate(a: SMC, b: SMC): Boolean = a.systemModification.date.compareTo(b.systemModification.date) > 0

  private def dateToString(smc: SMC): String = SystemModificationUpdate.dateFormat.format(smc.systemModification.date)

  private val dateCol = new Column[SMC]("Date", compareByDate _, (smc) => makePrimaryKeyHtml(dateToString(smc), smc.systemModification.systemModificationPK))

  //private val userCol = new Column[SMC]("User", _.user.id, (smc) => wrapAlias(smc.user.id))

  private val summaryCol = new Column[SMC](
    "Summary", _.systemModification.summary, (smc) => <div>{ WebUtil.firstPartOf(smc.systemModification.summary, 120) }</div>)

  private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

  //override val columnList = Seq(dateCol, userCol, summaryCol)
  override val columnList = Seq(dateCol, summaryCol)

  override def getData(valueMap: ValueMapT, response: Response) = SystemModification.listWithDependencies.sortBy(_.systemModification.date.getTime)

  override def getPK(smc: SMC): Long = smc.systemModification.systemModificationPK.get
}
