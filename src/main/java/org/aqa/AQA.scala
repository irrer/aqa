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


package org.aqa

import edu.umro.ScalaUtil.PeriodicRestart
import org.aqa.db.DbSetup
import org.aqa.run.RunProcedure
import org.aqa.web.WebServer

import java.util.Date

/**
  * Main service entry point.  Start up major portions of
  * the service and catch unexpected errors.
  */
object AQA extends Logging {

  /** Time at which service was started. */
  val serviceStartTime: Long = System.currentTimeMillis

  def main(args: Array[String]): Unit = {

    try {
      println("AQA service starting at " + Util.timeHumanFriendly(new Date(serviceStartTime)))
      Util.showJarFile(this)

      println(edu.umro.ScalaUtil.Util.getJarPropertyFile(AQA.getClass))

      if (Config.validate) {
        DbSetup.init
        DbSetup.smokeTest
        // Run.handleRunningProcedureList
        RunProcedure.cleanupRunningProcedures
        new WebServer
        new PeriodicRestart(Config.RestartTime)
        logger.info("AQA service started")
      } else
        logger.error("Could not validate configuration")
    } catch {
      // Exceptions thrown to this level should not happen, and if they do it probably means that something
      // unexpected has happened.
      //
      // If there is a problem, catch and log the error, delay, and then exit with a failed status.  The
      // failed status will tell the service wrapper to restart the service.  The delay is there in the
      // event that this service behaves badly and keeps exiting when started, an keeps the service from
      // using excessive resources.
      case e: Exception =>
        logger.error("Unexpected exception in main: " + fmtEx(e))
        Thread.sleep(30 * 1000)
        System.exit(1)
      case t: Throwable =>
        logger.error("Unexpected throwable in main: " + fmtEx(t))
        Thread.sleep(30 * 1000)
        System.exit(2)
    }
  }

  /**
   * Initiate the restarting of the service. This service is configured to be restarted
   * (using YAJSW) to restart if it exits with status 1.
   */
  def initiateServiceRestart: Unit = {
    class InitiateServiceRestart extends Runnable {
      private val delay: Int = 4 * 1000 // wait long enough for web client to receive a response
      override def run(): Unit = {
        logger.info("Shutting down service for restart in " + delay + " ms...")
        Thread.sleep(delay)
        logger.info("Shutting down service now.")
        System.exit(1)
      }
      new Thread(this).start()
    }
    new InitiateServiceRestart
  }

}
