package org.aqa.web

import org.restlet.Response
import org.aqa.web.WebUtil._
import org.aqa.Config
import java.io.File

object PlatformUpdateList {
  private val path = new String((new PlatformUpdateList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)

}

class PlatformUpdateList extends GenericList[PlatformUpdate] with WebUtil.SubUrlAdmin {

  override val listName = "Platform Update"

  private val dateCol = new Column[PlatformUpdate]("Date", (pu) => PlatformUpdate.timeFormat.format(pu.date), (pu) => <div>{ PlatformUpdate.timeFormat.format(pu.date) }</div>)
  private val userCol = new Column[PlatformUpdate]("User", _.user, (pu) => <div>{ pu.user }</div>)
  private val summaryCol = new Column[PlatformUpdate]("Summary", _.summary, (pu) => <div>{ pu.summary }</div>)

  private def optionToString[T](opt: Option[T]): String = if (opt.isDefined) opt.get.toString else "none";

  override val columnList = Seq(dateCol, userCol, summaryCol)

  override def getData(valueMap: ValueMapT, response: Response) = PlatformUpdate.readAll

  override def getPK(platformUpdate: PlatformUpdate): Long = platformUpdate.date.getTime
}
