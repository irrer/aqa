package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Institution
import org.aqa.web.WebUtil._

object InstitutionList {    
    private val path = new String((new InstitutionList).pathOf)
    
    def redirect(response: Response) = redirectSeeOthr(response, path)
}

class InstitutionList extends GenericList[Institution] with WebUtil.SubUrlAdmin {

    private def humanReadableURL(url: String): String = {
        val small = url.replaceAll("^https://", "").replaceAll("^http://", "").replaceAll("^www\\.", "")
        WebUtil.firstPartOf(small, 20)
    }

    private def urlHTML(institution: Institution): Elem = {
        val url = institution.url.trim
        if (url.size > 0)
            <a href={ url } target="_blank" title={ url }>{ humanReadableURL(url) }</a>
        else <div></div>
    }

    private def descriptionHTML(institution: Institution): Elem = {
        <div> { WebUtil.firstPartOf(institution.description, 60) } </div>
    }

    private val idCol = new Column[Institution]("Name", _.name, (inst) => makePrimaryKeyHtml(inst.name, inst.institutionPK))

    private val urlCol = new Column[Institution]("URL", _.url, urlHTML)

    private val descriptionCol = new Column[Institution]("Description", _.description, descriptionHTML)

    override def listName = "Institution"

    override val columnList = Seq(idCol, urlCol, descriptionCol)

    override def getData(valueMap: ValueMapT) = Institution.list

    override def getPK(value: Institution): Long = value.institutionPK.get
}