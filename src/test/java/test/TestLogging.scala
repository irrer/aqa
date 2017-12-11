package test

//import java.util.logging.Level
//import java.util.logging.FileHandler
//import java.util.logging.Logger
//import edu.umro.util.Log
import org.restlet.Restlet
//import org.restlet.routing.Filter
//import org.aqa.Logging

//import java.io.File

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.aqa.db.DbSetup

object TestLogging {

    def main(args: Array[String]): Unit = {
        println("Logging starting")
        //        System.setProperty("log4j2.debug", "true");
        //System.setProperty("java.util.logging.config.file", """D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\log4j.properties""");
        //        System.setProperty("log4j.configurationFile", """D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\log4j.properties""");
        System.setProperty("log4j2.configurationFile", """D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\log4j2.xml""");

        val logger: Logger = LoggerFactory.getLogger(this.getClass);

        logger.trace("trace log")
        logger.debug("debug log")
        logger.info("info log")
        logger.info("warn log")
        logger.error("error log")
        DbSetup.init
        DbSetup.smokeTest

        class Foo extends Restlet {
            val logger: Logger = LoggerFactory.getLogger(this.getClass);
            def wrn = logger.warn("Warning from foo")
        }
        val foo = new Foo
        foo.wrn

        // Logging.logSevere("This is a severe hello from logging!")
        Thread.sleep(1000)
        println("Logging is done")
        System.exit(0)
    }

}