package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._

// TODO can this file be removed?

object XRunProcedureList {
    private val path = new String((new XRunProcedureList).pathOf)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class XRunProcedureList extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlRun {

    type PU = Procedure.ProcedureUser
    override def listName = "Procedure"

    private def notesHTML(pu: PU): Elem = <div>{ WebUtil.firstPartOf(pu.procedure.notes, 60) }</div>

    private val idCol = new Column[PU]("Name", _.procedure.name, (pu) => makePrimaryKeyHtml(pu.procedure.name, pu.procedure.procedurePK))

    private val versionCol = new Column[PU]("Version", _.procedure.version)

    private val notesCol = new Column[PU]("Notes", _.procedure.notes, notesHTML)

    override val columnList = Seq(idCol, versionCol, notesCol)

    override def getData(valueMap: ValueMapT) = Procedure.listWithDependencies

    override def getPK(value: PU): Long = value.procedure.procedurePK.get
}
