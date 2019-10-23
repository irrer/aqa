package org.aqa.web

import java.util.Date
import scala.xml.Elem
import java.text.SimpleDateFormat
import java.io.File
import org.aqa.Logging
import org.aqa.Util
import scala.xml.XML
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil

/**
 * Encapsulate a platform update.
 *
 * @param date: When the update was put on the platform.
 *
 * @param user: User who performed the update
 *
 * @param summary: Short description of update.
 *
 * @param description: Full description of update.
 */
case class PlatformUpdate(date: Date, user: String, summary: String, description: Elem) extends Logging {

  def toHtml: Elem = {
    <div>
      <div class="row">
        <div class="col-md-2">
          Date:{ PlatformUpdate.timeFormat.format(date) }
        </div>
        <div class="col-md-2">
          User:{ user }
        </div>
        <div class="row">
          Description:
          <br>
            { description }
          </br>
        </div>
      </div>
    </div>
  }

  private val file = {
    val name = FileUtil.replaceInvalidFileNameCharacters(Util.standardDateFormat.format(date), '_')
    new File(PlatformUpdate.dir, name)
  }

  def persist = {
    val elem: Elem = {
      <PlatformUpdate>
        <date>{ Util.standardDateFormat.format(date) }</date>
        <user>{ user.trim }</user>
        <summary>{ summary }</summary>
        <description>{ description }</description>
      </PlatformUpdate>
    }

    val text = WebUtil.xmlToText(elem)

    file.getParentFile.mkdirs
    Util.writeFile(file, text)

    logger.info("Wrote PlatformUpdate file " + file.getAbsolutePath + "\n" + text)
  }

}

object PlatformUpdate extends Logging {

  val timeFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm")

  /**
   * Convert a file to a PlatformUpdate.  If there is an error then report it and return None.
   */
  private def fileToPlatformUpdate(file: File): Option[PlatformUpdate] = {
    try {
      val content = XML.loadFile(file)
      val date = Util.standardDateFormat.parse((content \ "date").head.text.toString.trim)
      val user = (content \ "user").head.text.toString.trim
      val summary = (content \ "summary").head.text.toString.trim
      val description = (content \ "description").head.asInstanceOf[Elem]
      Some(new PlatformUpdate(date, user, summary, description))
    } catch {
      case t: Throwable => {
        logger.warn("Unable to read file as a PlatformUpdate: " + file.getAbsolutePath + " : " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Main directory containing all PlatformUpdate files.
   */
  def dir = new File(Config.DataDir, "PlatformUpdateList")

  /**
   * Read all PlatformUpdate files.
   */
  def readAll: Seq[PlatformUpdate] = {
    dir.mkdirs
    dir.listFiles.toSeq.map(file => fileToPlatformUpdate(file)).flatten
  }

}