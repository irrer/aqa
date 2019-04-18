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

  override def afterHandle(request: Request, response: Response): Unit = {
    val remaining = request.getResourceRef.getRemainingPart
    if (remaining.toLowerCase.startsWith("/doc/") && remaining.toLowerCase.endsWith(".html")) {
      super.afterHandle(request, response)
      val oldText = response.getEntityAsText

      val newText = Doc.wrapperContent.replace(Doc.contentTag, oldText).replaceAll(Doc.titleTag, getTitleFromText(oldText))
      response.setEntity(newText, MediaType.TEXT_HTML)
    }
  }

}

