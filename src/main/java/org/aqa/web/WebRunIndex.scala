package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Procedure
import org.restlet.Request
import org.aqa.web.WebUtil._
import java.util.Date

object WebRunIndex {
    private val path = new String((new WebRunIndex).pathOf)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class WebRunIndex extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlRun {

    override val pageTitle = "Run Procedures"

    override val canCreate = false

    override def getData(valueMap: ValueMapT) = Procedure.listWithDependencies
    override val listName = "Procedure"

    type PU = Procedure.ProcedureUser

    private def notesHTML(pu: PU): Elem = <div> { WebUtil.firstPartOf(pu.procedure.notes, 60) } </div>

    private def nameCompare(a: PU, b: PU) = a.procedure.name.compareTo(b.procedure.name) < 0

    private def nameToHtml(pu: PU): Elem = {
        // <a href={ SubUrl.url(subUrl, pu.procedure.webUrl) + "?" + sessionLabel + "=" + Session.makeUniqueId }>{ pu.procedure.name }</a>
        <a href={ SubUrl.url(subUrl, pu.procedure.webUrl) }>{ pu.procedure.name }</a>
    }

    private val nameCol = new Column[PU]("Name", _.procedure.name, nameToHtml _) // (pu) => makePrimaryKeyHtml(pu.procedure.name, pu.procedure.procedurePK))

    private val versionCol = new Column[PU]("Version", _.procedure.version)

    private val notesCol = new Column[PU]("Notes", _.procedure.notes, notesHTML)

    override val columnList = Seq(nameCol, versionCol, notesCol)

    override def getPK(value: PU): Long = value.procedure.procedurePK.get

    private def getProcedure(valueMap: ValueMapT): Option[Procedure] = {
        val procedurePK = valueMap.get(ProcedureUpdate.procedurePKTag)

        if (procedurePK.isDefined) Procedure.get(procedurePK.get.toLong)

        else None
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        val procedure = getProcedure(valueMap)
        //if (procedure.isDefined) WebRun.getWebRun(procedure.get).handle(valueMap, request, response)
        //else 
        get(valueMap, response)
    }

}
