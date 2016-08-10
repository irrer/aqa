package org.aqac.web

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
import org.aqac.Logging._
import scala.concurrent.Await
import org.aqac.db.Institution
import org.aqac.db.User

class Column[VL](val columnName: String, val getValue: (VL) => Comparable[_ <: Any], val makeHTML: (VL) => Elem) {
    def this(columnName: String, getValue: (VL) => Comparable[_ <: Any]) = this(columnName, getValue, null)
}

abstract class GenericList[VL](val listName: String, columnList: Seq[Column[VL]]) extends Restlet with SubUrl {

    val pageTitle = "List " + listName + "s"

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
    def getPK(value: VL): Long; // must be overridden 

    /**
     * Retrieve data, usually from the database.
     */
    def getData: Seq[VL]; // must be overridden 

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

    protected def formatValueWithLink(value: VL, column: Column[VL]): Elem = {
        <td><a href={ updatePath + "?" + getPKName + "=" + getPK(value) }>{ column.getValue(value) }</a></td>
    }

    private def makeRow(value: VL): Elem = {
        <tr>
            { if (columnList.head.makeHTML == null) formatValueWithLink(value, columnList(0)) else columnList.head.makeHTML(value) }
            { columnList.tail.map(col => {
                if (col.makeHTML == null) <td>{ col.getValue(value).toString }</td> else col.makeHTML(value)}) 
            }
        </tr>
    }

    /**
     * Get the column on which to sort.  If there is no column specified or
     * the column is invalid, then sort on column 0.
     */
    private def getSortColumn(valueMap: Map[String, String]): Int = {
        val sortText = valueMap.get("sort")
        if ((!sortText.isEmpty) && (sortText.get.toInt >= 0) && (sortText.get.toInt < columnList.size)) sortText.get.toInt else 0
    }

    private def sortedData(valueMap: Map[String, String]): Seq[VL] = {
        val data = getData

        val column = getSortColumn(valueMap)

        def compare(rowA: VL, rowB: VL): Boolean = {
            val a = columnList(column).getValue(rowA)
            val b = columnList(column).getValue(rowB)

            val compareValue =
                if (a.isInstanceOf[String] && b.isInstanceOf[String])
                    a.toString.toLowerCase.compareTo(b.toString.toLowerCase)
                else a.asInstanceOf[Comparable[Any]].compareTo(b.asInstanceOf[Comparable[Any]])

            compareValue <= 0
        }

        data.sortWith((rowA, rowB) => compare(rowA, rowB))
    }

    private val createNew: Elem = {
        <div class="row col-md-2 col-md-offset-10">
            <strong><a href={ updatePath }>Create new { listName }</a><p/></strong>
        </div>;
    }

    protected def makeForm(valueMap: ValueMapT): Elem = {
        <div class="row col-md-10 col-md-offset-1">
            <h1>{ pageTitle }</h1>
            { if (canCreate) createNew }
            <table class="table table-striped">
                <tbody>
                    { tableHead }
                    { sortedData(valueMap).map(value => makeRow(value)) }
                </tbody>
            </table>
        </div>;
    }

    protected def get(valueMap: Map[String, String], response: Response) = {
        val text = wrapBody(makeForm(valueMap), pageTitle)
        setResponse(text, response, Status.SUCCESS_OK)
    }
    
    protected def beforeHandle(valueMap: ValueMapT, request: Request, response: Response): Unit = {
        println("hey") // TODO rm
    }

    override def handle(request: Request, response: Response): Unit = {
        super.handle(request, response)
        val valueMap = getValueMap(request)
        beforeHandle(valueMap, request, response)
        request.getMethod match {
            case Method.GET => get(valueMap, response)
            case _ => WebUtil.notFound(response)
        }
        if (true) {
            response.getEntity.setExpirationDate(new Date(0))
        }
    }
}
