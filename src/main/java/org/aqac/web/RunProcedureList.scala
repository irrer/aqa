package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.Procedure

object RunProcedureList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.run, ProcedureList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class RunProcedureList extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlRun {

    type PU = Procedure.ProcedureUser
    override def listName = "Procedure"

    private def notesHTML(pu: PU): Elem = <div>{ WebUtil.firstPartOf(pu.procedure.notes, 60) }</div>

    private val idCol = new Column[PU]("Name", _.procedure.name, (pu) => makePrimaryKeyHtml(pu.procedure.name, pu.procedure.procedurePK))

    private val versionCol = new Column[PU]("Version", _.procedure.version)

    private val notesCol = new Column[PU]("Notes", _.procedure.notes, notesHTML)

    override val columnList = Seq(idCol, versionCol, notesCol)

    override def getData = Procedure.listWithDependencies

    override def getPK(value: PU): Long = value.procedure.procedurePK.get
}
