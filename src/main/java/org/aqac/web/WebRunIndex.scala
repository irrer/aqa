package org.aqac.web

import org.restlet.Response
import scala.xml.Elem
import org.aqac.db.Procedure
import org.restlet.Request
import WebUtil._
import java.util.Date

object WebRunIndex {

    val path = WebUtil.pathOf(WebUtil.SubUrl.run, WebRunIndex.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)

    type PU = Procedure.PU

    private def notesHTML(pu: PU): Elem = <td> { WebUtil.firstPartOf(pu._1.notes, 60) } </td>

    private val nameCol = new Column[PU]("Name", _._1.name)

    private val versionCol = new Column[PU]("Version", _._1.version)

    private val notesCol = new Column[PU]("Notes", _._1.notes, notesHTML)

    val colList = Seq(nameCol, versionCol, notesCol)
}

class WebRunIndex extends GenericList[Procedure.PU]("Procedure", WebRunIndex.colList) with WebUtil.SubUrlRun {

    override val pageTitle = "Run Procedures"

    override val canCreate = false

    override def getData = Procedure.listWithDependencies

    private type VL = WebRunIndex.PU

    override def getPK(value: VL): Long = value._1.procedurePK.get

    override def formatValueWithLink(value: VL, column: Column[VL]): Elem = {
        <td><a href={ value._1.webUrl }>{ column.getValue(value) }</a></td>
    }

    private def getProcedure(valueMap: ValueMapT): Option[Procedure] = {
        val procedurePK = valueMap.get(ProcedureUpdate.procedurePKTag)

        if (procedurePK.isDefined) Procedure.get(procedurePK.get.toLong)

        else None
    }

    override def handle(request: Request, response: Response): Unit = {
        val valueMap = getValueMap(request)
        val procedure = getProcedure(valueMap)
        //if (procedure.isDefined) WebRun.getWebRun(procedure.get).handle(valueMap, request, response)
        //else 
        get(valueMap, response)
    }

}
