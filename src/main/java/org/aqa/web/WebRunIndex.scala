package org.aqa.web

import org.restlet.Response
import scala.xml.Elem
import org.aqa.db.Procedure
import org.restlet.Request
import org.aqa.web.WebUtil._
import java.util.Date
import org.restlet.data.Status
import org.restlet.data.MediaType

object WebRunIndex {
  private val path = new String((new WebRunIndex).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)
}

class WebRunIndex extends GenericList[Procedure.ProcedureUser] with WebUtil.SubUrlRun {

  override val pageTitle = "Run Procedures"

  override val canCreate = false

  override def getData(valueMap: ValueMapT, response: Response) = Procedure.listWithDependencies
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

  /**
   * Respond to the client by giving an XML list of procedure URLs.
   */
  private def respondWithXmlList(response: Response) = {
    def puToXml(pu: PU): Elem = {
      <Run Version={ pu.procedure.version } Name={ pu.procedure.name } URL={ SubUrl.url(subUrl, pu.procedure.webUrl) }/>
    }
    val data = getData(emptyValueMap, response)

    val xml = {
      <RunList>
        { data.map(pu => puToXml(pu)) }
      </RunList>
    }

    response.setStatus(Status.SUCCESS_OK)
    response.setEntity(xmlToText(xml), MediaType.APPLICATION_XML)
  }

  
  /**
   * Show a list of procedures to run.
   * 
   * If the value : list=true is in the URL, then an XML list of procedures with
   * their URLS will be returned.  This is to support the AQA client.
   */
  override def handle(request: Request, response: Response): Unit = {
    if (request.getOriginalRef.toString.toLowerCase.contains("list="))
      respondWithXmlList(response)
    else
      super.handle(request, response)
    //    val valueMap = getValueMap(request)
    //    val procedure = getProcedure(valueMap)
    //    get(valueMap, response)
  }

}
