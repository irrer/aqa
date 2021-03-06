/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.web

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.MediaType
import org.restlet.data.Status
import org.restlet.routing.Filter

import java.io.File

/**
  * Support for internal documentation.  HTML files in the static/doc directory are wrapped with
  * standard HTML (in static/doc/wrapper.html) and the title of the document set.
  *
  * Having a standard wrapper gives a common look and feel to the documentation.  Putting it in a
  * file makes it maintainable and consistent across multiple documents.
  */
object Doc {
  private val path = new String((new Doc).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)

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
  // lazy val wrapperFile = new File(docDir, "wrapper.html")

  lazy val wrapperContent: String = {
    val content = { <div class="col-md-8 col-md-offset-1">{contentTag}</div> }
    WebUtil.wrapBody(content, titleTag, mathjax = true)
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
      val begin = text.substring(start + tag.length)
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
    val rem = remaining.toLowerCase
    if (rem.startsWith("/doc/") && (rem.endsWith(".xml") || rem.endsWith(".html"))) {
      super.afterHandle(request, response)
      val sourceText = response.getEntityAsText

      Util.loadXml(sourceText) match {
        case Left(msg) =>
          logger.warn(
            "Unexpected error.  User requested doc " + remaining +
              " but it was not valid XML.  Content will still be returned but may be misinterpreted by the web browser.\nError: " + msg
          )
        case _ =>
      }
      val newText = Doc.wrapperContent.replace(Doc.contentTag, sourceText).replaceAll(Doc.titleTag, getTitleFromText(sourceText))

      response.getEntity.exhaust // not totally sure if this is required or not
      response.getEntity.release() // not totally sure if this is required or not
      response.setEntity(newText, MediaType.TEXT_HTML)
      response.setStatus(Status.SUCCESS_OK)
    }
  }

}
