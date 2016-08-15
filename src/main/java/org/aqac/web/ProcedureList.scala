package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.Procedure

object ProcedureList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, ProcedureList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class ProcedureList extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlAdmin {

    override val listName = "Procedure"

    type PU = Procedure.ProcedureUser

    private def notesHTML(pu: PU): Elem = <div>{ WebUtil.firstPartOf(pu.procedure.notes, 60) }</div>

    private val idCol = new Column[PU]("Name", _.procedure.name, (inst) => makePrimaryKeyHtml(inst.procedure.name, inst.procedure.procedurePK))

    private val supportedByCol = new Column[PU]("Supported By", _.user.fullName)

    private val versionCol = new Column[PU]("Version", _.procedure.version)

    private val notesCol = new Column[PU]("Notes", _.procedure.notes, notesHTML)

    override val columnList = Seq(idCol, supportedByCol, versionCol, notesCol)

    override def getData = Procedure.listWithDependencies

    override def getPK(value: PU): Long = value.procedure.procedurePK.get
}
