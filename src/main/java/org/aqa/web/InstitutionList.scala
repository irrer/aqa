package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Institution
import org.aqa.web.WebUtil._
import org.aqa.AnonymizeUtil

object InstitutionList {
  private val path = new String((new InstitutionList).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class InstitutionList extends GenericList[Institution] with WebUtil.SubUrlAdmin {

  private def humanReadableURL(url: String): String = {
    val small = url.replaceAll("^https://", "").replaceAll("^http://", "").replaceAll("^www\\.", "")
    WebUtil.firstPartOf(small, 20)
  }

  private def urlHTML(institution: Institution): Elem = {
    val url = institution.url_real.trim
    if (url.size > 0)
      <a href={ url } target="_blank" title={ url }>{ humanReadableURL(url) }</a>
    else <div></div>
  }

  private def descriptionHTML(institution: Institution): Elem = {
    <div> { WebUtil.firstPartOf(institution.description_real, 60) } </div>
  }

  private val idCol = new Column[Institution]("Name", _.name, (inst) => makePrimaryKeyHtmlWithAQAAlias(inst.name, inst.institutionPK))

  private val urlCol = encryptedColumn("URL", AnonymizeUtil.institutionAliasUrlPrefixId, (inst) => inst.institutionPK.get)

  new Column[Institution]("URL", _.url_real, urlHTML)

  private val descriptionCol = encryptedColumn("Description", AnonymizeUtil.institutionAliasDescriptionPrefixId, (inst) => inst.institutionPK.get)

  override def listName = "Institution"

  override val columnList = Seq(idCol, urlCol, descriptionCol)

  override def getData(valueMap: ValueMapT, response: Response) = Institution.list

  override def getPK(value: Institution): Long = value.institutionPK.get
}
