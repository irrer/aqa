package org.aqa.web

import edu.umro.ScalaUtil.Trace._
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import org.aqa.Config

/**
 * Provide a session id that will be considered to be unique.  Designed to
 * be used for user sessions to keep them distinct from each other.  The
 * current time is used both for uniqueness and as a debugging/diagnostic
 * aid.
 */

object Session {

  /**
   * Make a unique session id.
   */
  def makeUniqueId: String =
    Session.synchronized({
      val sessionFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")
      val start = System.currentTimeMillis
      while (System.currentTimeMillis == start) Thread.sleep(1)
      val s = sessionFormat.format(new Date)
      s
    })

  def idToFile(id: String): File = new File(Config.tmpDirFile, id)

  def main(args: Array[String]): Unit = {
    for (i <- (0 until 5)) println(makeUniqueId)
  }
}
