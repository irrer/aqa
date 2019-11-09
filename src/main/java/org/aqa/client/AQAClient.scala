package org.aqa.client

import org.aqa.Logging
import org.aqa.Util
import java.util.Date
import edu.umro.ScalaUtil.PeriodicRestart

/**
 * Main entry point for client service.
 */
object AQAClient extends Logging {
  /** Time at which service was started. */
  val serviceStartTime = System.currentTimeMillis

  def main(args: Array[String]): Unit = {

    try {
      println("AQAClient starting at " + Util.timeHumanFriendly(new Date(serviceStartTime)))
      Util.showJarFile(this)

      if (ClientConfig.validate) {
        DicomRetrieve.init
        WebUpload.init
        new ClientWebServer
        new PeriodicRestart(ClientConfig.RestartTime)
        logger.info("AQAClient started")
      }
    } catch {
      // Exceptions thrown to this level should not happen, and if they do it probably means that something
      // very unexpected has happened.
      //
      // If there is a problem, catch and log the error, delay, and then exit with a failed status.  The
      // failed status will tell the service wrapper to restart the service.  The delay is there in the
      // event that this service behaves badly and keeps exiting when started, an keeps the service from
      // using excessive resources.
      case e: Exception => {
        logger.error("Unexpected exception in main: " + fmtEx(e))
        Thread.sleep(30 * 1000)
        System.exit(1)
      }
      case t: Throwable => {
        logger.error("Unexpected throwable in main: " + fmtEx(t))
        Thread.sleep(30 * 1000)
        System.exit(2)
      }
    }
  }
}