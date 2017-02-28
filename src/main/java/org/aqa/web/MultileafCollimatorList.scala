package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.MultileafCollimator

object MultileafCollimatorList {
    val path = WebUtil.pathOf(WebUtil.SubUrl.admin, MultileafCollimatorList.getClass.getName)

    def redirect(response: Response) = response.redirectSeeOther(path)
}

class MultileafCollimatorList extends GenericList[MultileafCollimator] with WebUtil.SubUrlAdmin {
    override def listName = "MultileafCollimator"

    override def getData = MultileafCollimator.list

    override def getPK(value: MultileafCollimator): Long = value.multileafCollimatorPK.get

    private def notesHTML(machineType: MultileafCollimator): Elem = <div>{ WebUtil.firstPartOf(machineType.notes, 60) }</div>

    private val manufacturerCol = new Column[MultileafCollimator]("Manufacturer", _.manufacturer, (mlc) => makePrimaryKeyHtml(mlc.manufacturer, mlc.multileafCollimatorPK))

    private val modelCol = new Column[MultileafCollimator]("Model", _.model)

    private val versionCol = new Column[MultileafCollimator]("Version", _.version)

    private val notesCol = new Column[MultileafCollimator]("Notes", _.notes, notesHTML)

    override val columnList = Seq(manufacturerCol, modelCol, versionCol, notesCol)
}
