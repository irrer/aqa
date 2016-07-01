package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.Procedure

object RunProcedureList {

    val path = WebUtil.pathOf(ProcedureList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)

    type PU = Procedure.PU

    private def notesHTML(pu: PU): Elem = <td> { WebUtil.firstPartOf(pu._1.notes, 60) } </td>

    private val nameCol = new Column[PU]("Name", _._1.name)

    private val versionCol = new Column[PU]("Version", _._1.version)

    private val notesCol = new Column[PU]("Notes", _._1.notes, notesHTML)

    val colList = Seq(nameCol, versionCol, notesCol)

}

class RunProcedureList extends GenericList[Procedure.PU]("Procedure", ProcedureList.colList) {

    override def getData = Procedure.listWithDependencies

    override def getPK(value: Procedure.PU): Long = value._1.procedurePK.get
}
