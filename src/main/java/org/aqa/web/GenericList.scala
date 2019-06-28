package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import org.aqa.db.Institution
import org.aqa.db.User
import org.aqa.AnonymizeUtil

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

  protected val columnList: Seq[Column[VL]];

  /** Default time in seconds for page refresh. */
  val defaultPageRefreshTime: Option[Int] = Some(3600)

  def pageTitle = "List " + listName + "s"

  def updatePath = SubUrl.url(subUrl, listName + "Update")
  def listPath = SubUrl.url(subUrl, listName + "List")

  /** Override with false to prevent showing 'Create new' */
  protected val canCreate: Boolean = true

  /**
   * Get the name of the primary key to be used in directing user to the update page.
   */
  def getPKName: String = listName.toString.substring(0, 1).toLowerCase + listName.substring(1) + "PK"

  /**
   * Get the primary key used to direct the user to the corresponding update page.
   */
  protected def getPK(value: VL): Long; // must be overridden

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

  protected def createNew(valueMap: ValueMapT): Elem = {
    <div class="row col-md-2 col-md-offset-10">
      <strong><a href={ createNewPath(valueMap) }>Create new { listName }</a><p> </p></strong>
    </div>;
  }

  protected def makeForm(valueMap: ValueMapT, response: Response): Elem = {

    <div class="row col-md-10 col-md-offset-1">
      <h1>{ pageTitle }</h1>
      { if (canCreate) createNew(valueMap) }
      <table class="table table-striped">
        <tbody>
          { tableHead }
          { getData(valueMap, response).sortWith(columnList(getSortColumn(valueMap)).compare).map(value => makeRow(value)) }
        </tbody>
      </table>
    </div>;
  }

  protected def get(valueMap: ValueMapT, response: Response) = {
    val text = wrapBody(makeForm(valueMap, response), pageTitle, defaultPageRefreshTime)
    setResponse(text, response, Status.SUCCESS_OK)
  }

  protected def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Unit = {
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
    beforeHandle(valueMap, request, response)
    request.getMethod match {
      case Method.GET => get(valueMap, response)
      case _ => WebUtil.notFound(response)
    }
    response.getEntity.setExpirationDate(new Date(0))
  }
}
