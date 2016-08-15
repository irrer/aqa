package org.aqa

import org.aqa.db.DbSetup
import org.aqa.web.WebServer
import org.aqa.run.Run
import org.aqa.Config

object AQA {
    def main(args: Array[String]): Unit = {
        println("Service starting")
        if (Config.validate) {
            DbSetup.init
            Run.handleRunningProcedureList
            new WebServer
        }
        println("Service started")
    }
}