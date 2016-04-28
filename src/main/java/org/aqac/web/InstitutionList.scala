package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.Institution

object InstitutionList {
    val path = WebUtil.pathOf(InstitutionList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)

    private def humanReadableURL(url: String): String = {
        val small = url.replaceAll("^https://", "").replaceAll("^http://", "").replaceAll("^www\\.", "")
        WebUtil.firstPartOf(small, 20)
    }

    private def urlHTML(institution: Institution): Elem = {
        val url = institution.url.trim
        if (url.size > 0)
            <td><a href={ url } target="_blank" title={ url }>{ humanReadableURL(url) }</a></td>
        else <td></td>
    }

    private def notesHTML(institution: Institution): Elem = {
        <td> { WebUtil.firstPartOf(institution.notes, 60) } </td>
    }

    private val nameCol = new Column[Institution]("Name", _.name)

    private val urlCol = new Column[Institution]("URL", _.url, urlHTML)

    private val notesCol = new Column[Institution]("Notes", _.notes, notesHTML)

    val colList = Seq(nameCol, urlCol, notesCol)
}

class InstitutionList extends GenericList[Institution]("Institution", InstitutionList.colList) {

    override def getData = Institution.list

    override def getPK(value: Institution): Long = value.institutionPK.get
}