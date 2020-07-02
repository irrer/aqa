package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import org.aqa.db.Institution
import org.aqa.db.User
import org.aqa.AnonymizeUtil
import edu.umro.ScalaUtil.Trace
import org.restlet.routing.Filter

case class Column[VL](columnName: String, compare: (VL, VL) => Boolean, makeHTML: (VL) => Elem) {

  def this(columnName: String, getValue: (VL) => String) =
    this(
      columnName,
      (a: VL, b: VL) => { getValue(a).compareTo(getValue(b)) < 0 },
      (a: VL) => <div>{ getValue(a) }</div>)

  def this(columnName: String, getValue: (VL) => String, makeHTML: (VL) => Elem) =
    this(
      columnName,
      (a: VL, b: VL) => { getValue(a).compareTo(getValue(b)) < 0 },
      makeHTML)
}

/**
 * Abstract class for web list support.
 */

abstract class GenericList[VL] extends Restlet with SubUrlTrait {

  protected def listName: String;

  def listNameNoBlank = listName.replace(" ", "")

  protected val columnList: Seq[Column[VL]];

  /** Default time in seconds for page refresh. */
  val defaultPageRefreshTime: Option[Int] = Some(3600)

  def pageTitle = "List " + listName + "s"

  def updatePath = SubUrl.url(subUrl, listNameNoBlank + "Update")
  def listPath = SubUrl.url(subUrl, listNameNoBlank + "List")

  /** Override with false to prevent showing 'Create new' */
  protected val canCreate: Boolean = true

  /**
   * Get the name of the primary key to be used in directing user to the update page.
   */
  def getPKName: String = listNameNoBlank.toString.substring(0, 1).toLowerCase + listNameNoBlank.substring(1) + "PK"

  /**
   * Get the primary key used to direct the user to the corresponding update page.
   */
  protected def getPK(value: VL): Long; // must be overridden

  /**
   * Optional list of HTML fields customized by the type of list.
   */
  protected def htmlFieldList(valueMap: ValueMapT): List[WebRow] = List[WebRow]();

  /**
   * Retrieve data, usually from the database.
   */
  protected def getData(valueMap: ValueMapT, response: Response): Seq[VL]; // must be overridden

  def columnToHeader(index: Int, column: Column[VL]): Elem = {
    <th class={ "col" + index + " header" }>
      <a href={ listPath + "?sort=" + index }>{ column.columnName }</a>
    </th>
  }

  private def tableHead: Elem = {
    <thead>
      <tr>
        { (0 until columnList.size).map(c => columnToHeader(c, columnList(c))) }
      </tr>
    </thead>
  }

  protected def makePrimaryKeyHtml(name: String, pk: String): Elem = {
    <a href={ updatePath + "?" + getPKName + "=" + pk }>{ name }</a>
  }

  protected def makePrimaryKeyHtml(name: String, pk: Option[Long]): Elem = makePrimaryKeyHtml(name, pk.get.toString)

  protected def makePrimaryKeyHtmlWithAQAAlias(name: String, pk: String): Elem = {
    <a href={ updatePath + "?" + getPKName + "=" + pk } aqaalias="">{ name }</a>
  }

  protected def makePrimaryKeyHtmlWithAQAAlias(name: String, pk: Option[Long]): Elem = makePrimaryKeyHtmlWithAQAAlias(name, pk.get.toString)

  private def td(elem: Elem): Elem = { <td>{ elem }</td> }

  private def makeRow(value: VL): Elem = {
    <tr>
      { columnList.map(col => td(col.makeHTML(value))) }
    </tr>
  }

  /**
   * Get the column on which to sort.  If there is no column specified or
   * the column is invalid, then sort on column 0.
   */
  private def getSortColumn(valueMap: ValueMapT): Int = {
    val sortText = valueMap.get("sort")
    if ((!sortText.isEmpty) && (sortText.get.toInt >= 0) && (sortText.get.toInt < columnList.size)) sortText.get.toInt else 0
  }

  /** Override this to customize the path (link) to follow to create a new list item. */
  protected def createNewPath(valueMap: ValueMapT): String = updatePath

  /**
   * Optionally show a link that lets users create a new item.
   */
  protected def createNew(valueMap: ValueMapT): Elem = {
    <strong><a href={ createNewPath(valueMap) }>Create new { listName }</a><p> </p></strong>
  }

  /**
   * Translate the rows to HTML.
   */
  private def tableElem(valueMap: ValueMapT, response: Response): Elem = {
    <table class="table table-striped">
      <tbody>
        { tableHead }
        { getData(valueMap, response).sortWith(columnList(getSortColumn(valueMap)).compare).map(value => makeRow(value)) }
      </tbody>
    </table>
  }

  /**
   * Field row that contains main table.
   */
  protected def tableRow(valueMap: ValueMapT, response: Response) = {
    val tableContent = new WebPlainText("tableContent", false, 12, 0, (valueMap) => tableElem(valueMap, response))
    val webRow: WebRow = List(tableContent)
    List(webRow)
  }

  /**
   * Field row that shows title and optionally 'can create' link.
   */
  protected def titleRow(valueMap: ValueMapT): List[WebRow] = {
    val title = Some(new WebPlainText("pageTitle", false, 9, 0, (_) => { <h1>{ pageTitle }</h1> }))
    val canCr = if (canCreate) Some(new WebPlainText("canCreate", false, 3, 0, createNew _)) else None
    val webRow = List(title, canCr).flatten.map(c => c.asInstanceOf[ToHtml]).toList
    List(webRow)
  }

  /**
   * Evaluate form fields, retrieve and filter list entries, and format into HTML.
   */
  protected def get(valueMap: ValueMapT, response: Response) = {
    val webRowList: List[WebRow] = titleRow(valueMap) ++ htmlFieldList(valueMap) ++ tableRow(valueMap, response)
    val form = new WebForm(listPath, webRowList)
    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

  protected def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Int = {
    Filter.CONTINUE
  }

  protected def encryptedColumn(title: String, prefix: String, primaryKey: (VL) => (Long)) = {
    new Column[VL](
      title,
      (vl: VL) => AnonymizeUtil.aliasify(prefix, primaryKey(vl)),
      (vl: VL) => wrapAlias(AnonymizeUtil.aliasify(prefix, primaryKey(vl))))
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    if (beforeHandle(valueMap, request, response) == Filter.CONTINUE) {
      request.getMethod match {
        case Method.GET => get(valueMap, response)
        case Method.POST => get(valueMap, response)
        case _ => WebUtil.notFound(response)
      }
      response.getEntity.setExpirationDate(new Date(0))
    }
  }
}
