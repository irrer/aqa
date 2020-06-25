package org.aqa

import java.util.logging.Level
import java.util.logging.FileHandler
import java.util.logging.Logger
import edu.umro.util.Log
import org.restlet.Restlet
import org.restlet.routing.Filter
import java.io.File

trait Logging {
  protected val logger = org.slf4j.LoggerFactory.getLogger("")

  /**
   * Format a <code>Throwable</code>.
   *
   * @param throwable Contains description and stack trace.
   *
   * @return Human readable version of <code>Throwable</code> and stack trace.
   */
  def fmtEx(throwable: Throwable): String = {
    val textList = throwable.getStackTrace.map(ste => "\n    " + ste) // convert to text
    textList.mkString // join as one string
  }
}

object LoggingMain extends Logging {

  //    private lazy val logDir: File = {
  //        val logFileName = Util.buildProperties.getProperty("wrapper.logfile")
  //        new File(logFileName).getParentFile
  //    }

  def main(args: Array[String]): Unit = {
    println("Logging starting")
    System.setProperty("log4j.rootLogger", "ALL")
    System.setProperty("log4j.logger.org.restlet", "WARN")
    System.setProperty("log4j.logger.org.springframework", "WARN")

    val simple = new org.apache.commons.logging.impl.SimpleLog("alfred")
    simple.warn("A warning")

    class Foo extends Restlet {
      def wrn = getLogger.warning("Warning from foo")
    }
    val foo = new Foo
    foo.wrn
    println("foo.getLogger: " + foo.getLogger)
    println("foo.getLogger.getName: " + foo.getLogger.getName)
    println("foo.getLogger.getParent.getName: " + foo.getLogger.getParent.getName)

    // val priority = new org.apache.spi.ErrorCode
    //        import org.apache.log4j.FileAppender
    //        val fileAppender = new FileAppender

    //org.apache.commons.logging.impl.SimpleLog.warn()

    // println("simple.dateTimeFormat: " + simple.)

    logger.error("Hello from logging!")
    println("LoggingMain done")
    System.exit(99)
  }

}