package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date
import scala.xml.Elem
import org.restlet.data.Parameter
import slick.lifted.TableQuery
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import org.aqa.db.EPID
import scala.concurrent.ExecutionContext.Implicits.global
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Form
import scala.xml.PrettyPrinter
import org.restlet.data.Status
import org.restlet.data.MediaType
import WebUtil._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.AQA
import org.aqa.Config
import edu.umro.util.OpSys
import org.restlet.routing.Filter
import edu.umro.ScalaUtil.Trace

/*
    try { Trace.trace(resRef.getIdentifier) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getBaseRef) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getExtensions) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getFragment) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getFragment(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getFragment(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getHierarchicalPart) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getHierarchicalPart(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getHierarchicalPart(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getIdentifier) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getIdentifier(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getIdentifier(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getLastSegment) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getLastSegment(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getLastSegment(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getMatrix) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getMatrix(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getMatrix(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getPath) } catch { case t: Throwable => Trace.trace("not available") } // /static/doc/SymmetryAndFlatness/index.html
    try { Trace.trace(resRef.getPath(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getPath(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getQuery) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getQuery(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getQuery(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getRelativePart) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getRelativePart(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getRelativePart(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getRemainingPart) } catch { case t: Throwable => Trace.trace("not available") } //   /doc/SymmetryAndFlatness/index.html
    try { Trace.trace(resRef.getRemainingPart(true)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getRemainingPart(false)) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getSegments) } catch { case t: Throwable => Trace.trace("not available") }
    try { Trace.trace(resRef.getTargetRef) } catch { case t: Throwable => Trace.trace("not available") }
    */

//val text = response.getEntityAsText
//Trace.trace("\n\n" + text + "\n\n")

object Doc {
  private val path = new String((new Doc).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)

  val contentTag = "@@content@@"

  val titleTag = "@@title@@"

  val docDirName = "doc"

  val defaultTitle = "User Guide"

  lazy val docDir = new File(Config.staticDirFile, docDirName)

  lazy val wrapperFile = new File(docDir, "wrapper.html")

  lazy val wrapperContent: String = {
    Util.readTextFile(wrapperFile) match {
      case Right(stuff) => stuff
      case _ => {
        val content = { <div>{ contentTag }</div> }
        WebUtil.wrapBody(content, defaultTitle)
      }
    }
  }

}

class Doc extends Filter with SubUrlAdmin with Logging {

  override def beforeHandle(request: Request, response: Response): Int = {
    val oldText = response.getEntityAsText
    //Trace.trace(oldText)
    Filter.CONTINUE
  }

  override def afterHandle(request: Request, response: Response): Unit = {
    println("\n\n\n\n")
    //Trace.trace(request)
    //Trace.trace(request.getHostRef)
    //Trace.trace(request.getRootRef)

    val resRef = request.getResourceRef

    val remaining = resRef.getRemainingPart
    if (remaining.toLowerCase.startsWith("/doc/") && remaining.toLowerCase.endsWith(".html")) {
      super.afterHandle(request, response)
      val oldText = response.getEntityAsText
      val entity = response.getEntity

      val title = {
        val tag = "title=\""
        val start = oldText.indexOf(tag)
        if (start == -1)
          Doc.defaultTitle
        else {
          val begin = oldText.substring(start + tag.size)
          val end = begin.indexOf('"')
          if (end == -1)
            Doc.defaultTitle
          else {
            val t = begin.substring(0, end)
            t
          }
        }
      }
      entity.exhaust
      entity.release

      val newText = Doc.wrapperContent.replace(Doc.contentTag, oldText).replaceAll(Doc.titleTag, title)

      response.setEntity(newText, MediaType.TEXT_HTML)
    }
  }

  //  def destroy(): Unit = ???
  //  def doFilter(x$1: javax.servlet.ServletRequest,x$2: javax.servlet.ServletResponse,x$3: javax.servlet.FilterChain): Unit = ???
  //  def init(x$1: javax.servlet.FilterConfig): Unit = ???

}

