package org.aqac

import org.aqac.db.DbSetup
import org.aqac.db.Institution
import javassist.tools.web.Webserver
import org.aqac.web.WebServer
import org.aqac.db.Db
import java.util.Date
import org.aqac.run.Run

object AQAC {
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

