package org.aqa

import org.aqa.db.DbSetup
import org.aqa.web.WebServer
import org.aqa.run.Run
import edu.umro.ScalaUtil.PeriodicRestart
import org.aqa.Logging._

/**
 * Main service entry point.
 */
object AQA {
    def main(args: Array[String]): Unit = {
        try {
            println("Service starting")
            if (Config.validate) {
                DbSetup.init
                Run.handleRunningProcedureList
                new WebServer
                new PeriodicRestart(Config.RestartTime)
                println("Service started")
            }
        }
        catch {
            // Exceptions thrown to this level should not happen, and if they do it probably means that something
            // very unexpected has happened.
            //
            // If there is a problem, catch and log the error, delay, and then exit with a failed status.  The
            // failed status will tell the service wrapper to restart the service.  The delay is there in the
            // event that this service behaves badly and keeps exiting when started, an keeps the service from
            // using excessive resources.
            case e: Exception => {
                logSevere("Unexpected exception in main: " + fmtEx(e))
                Thread.sleep(30 * 1000)
                System.exit(1)
            }
            case t: Throwable => {
                logSevere("Unexpected throwable in main: " + fmtEx(t))
                Thread.sleep(30 * 1000)
                System.exit(2)
            }
        }
    }
}