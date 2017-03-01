package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.EPID

object EPIDList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, EPIDList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class EPIDList extends GenericList[EPID] with WebUtil.SubUrlAdmin {
    override def listName = "EPID"

    override def getPKName = "epidPK"

    override def getData = EPID.list

    override def getPK(value: EPID): Long = value.epidPK.get

    private def notesHTML(machineType: EPID): Elem = <div>{ WebUtil.firstPartOf(machineType.notes, 60) }</div>

    private val manufacturerCol = new Column[EPID]("Manufacturer", _.manufacturer, (mlc) => makePrimaryKeyHtml(mlc.manufacturer, mlc.epidPK))

    private val modelCol = new Column[EPID]("Model", _.model)

    private val versionCol = new Column[EPID]("HW Version", _.hardwareVersion)

    private val notesCol = new Column[EPID]("Notes", _.notes, notesHTML)

    override val columnList = Seq(manufacturerCol, modelCol, versionCol, notesCol)
}
