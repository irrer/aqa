package org.aqa

import org.aqa.db.DbSetup
import org.aqa.web.WebServer
import org.aqa.run.Run
import edu.umro.ScalaUtil.PeriodicRestart
import java.io.File
import java.util.Date

/**
 * Main service entry point.  Start up major portions of
 * the service and catch unexpected errors.
 */
object AQA extends Logging {
  /** Time at which service was started. */
  val serviceStartTime = System.currentTimeMillis

  def main(args: Array[String]): Unit = {

    try {
      println("AQA Service starting at " + Util.timeHumanFriendly(new Date(serviceStartTime)))
      if (Config.validate) {
        DbSetup.init
        DbSetup.smokeTest
        DbSetup.storeFilesInDatabase // TODO remove when migration is complete
        Run.handleRunningProcedureList
        new WebServer
        new PeriodicRestart(Config.RestartTime)
        logger.info("Service started")
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

  def initiateServiceRestart = {
    class InitiateServiceRestart extends Runnable {
      val delay = 4 * 1000 // wait long enough for web client to receive a response
      def run = {
        logger.info("Shutting down service for restart in " + delay + " ms...")
        Thread.sleep(delay)
        logger.info("Shutting down service now.")
        System.exit(1)
      }
      (new Thread(this)).start
    }
    new InitiateServiceRestart
  }

}