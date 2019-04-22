package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import WebUtil._
import org.aqa.Logging
import org.aqa.Util
import java.io.File
import org.aqa.Config
import org.restlet.routing.Filter
import edu.umro.ScalaUtil.Trace
import scala.xml.XML
import scala.xml.Elem

/**
 * Support for internal documentation.  HTML files in the static/doc directory are wrapped with
 * standard HTML (in static/doc/wrapper.html) and the title of the document set.
 *
 * Having a standard wrapper gives a common look and feel to the documentation.  Putting it in a
 * file makes it maintainable and consistent across multiple documents.
 */
object Doc {
  private val path = new String((new Doc).pathOf)

  def redirect(response: Response) = response.redirectSeeOther(path)

  /** Replace this tag in the wrapper with the target content. */
  val contentTag = "@@content@@"

  /** Replace all instances of this with the content's title. */
  val titleTag = "@@title@@"

  /** Name of directory containing documentation. */
  val docDirName = "doc"

  /** Default title in case there is a problem finding the expected one. */
  val defaultTitle = "User Guide"

  lazy val docDir = new File(Config.staticDirFile, docDirName)

  /** Wrapper to include the content. */
  lazy val wrapperFile = new File(docDir, "wrapper.html")

  lazy val wrapperContent: String = {
    val content = { <div>{ contentTag }</div> }
    WebUtil.wrapBody(content, titleTag)

    //    Util.readTextFile(wrapperFile) match {
    //      case Right(stuff) => stuff
    //      case _ => {
    //        val content = { <div>{ contentTag }</div> }
    //        WebUtil.wrapBody(content, defaultTitle)
    //      }
    //    }
  }

}

class Doc extends Filter with SubUrlDoc with Logging {

  /**
   * Get the title from the HTML text by finding the first HTML title attribute and using
   * it's content.  The text 'Hello there' would be found in the following example:
   *
   * <div title="Hello there">
   *     blah blah ...
   */
  private def getTitleFromText(text: String): String = {
    val tag = "title=\""
    val start = text.indexOf(tag)
    if (start == -1)
      Doc.defaultTitle
    else {
      val begin = text.substring(start + tag.size)
      val end = begin.indexOf('"')
      if (end == -1)
        Doc.defaultTitle
      else {
        val t = begin.substring(0, end)
        t
      }
    }
  }

  private def processXml(doc: Elem): String = {
    val pageTitle = {
      (doc \ "@title").headOption match {
        case Some(node) => node.text
        case _ => Doc.defaultTitle
      }
    }
    WebUtil.wrapBody(doc, pageTitle).replace(Doc.titleTag, pageTitle)
  }

  override def afterHandle(request: Request, response: Response): Unit = {
    val remaining = request.getResourceRef.getRemainingPart
    val rem = remaining.toLowerCase
    if (rem.startsWith("/doc/") && (rem.endsWith(".xml") || rem.endsWith(".html"))) {
      super.afterHandle(request, response)
      val sourceText = response.getEntityAsText

      val newText = Util.loadXml(sourceText) match {
        case Right(doc) => processXml(doc)
        case Left(msg) => {
          logger.warn("User requested doc " + remaining + " but was not valid XML: " + msg)
          val j = Doc.wrapperContent
          Doc.wrapperContent.replace(Doc.contentTag, sourceText).replaceAll(Doc.titleTag, getTitleFromText(sourceText))
        }
      }

      //      response.getEntity.exhaust // not totally sure if this is required or not
      //      response.getEntity.release // not totally sure if this is required or not
      response.setEntity(newText, MediaType.TEXT_HTML)
    }
  }

}

