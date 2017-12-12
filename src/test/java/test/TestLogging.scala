package test

import org.restlet.Restlet

import org.slf4j.Logger
import org.slf4j.LoggerFactory
//import java.util.logging.Logger
import org.aqa.db.DbSetup
import org.aqa.Config
import org.aqa.web.WebServer

private object Loo {
    val logger: Logger = LoggerFactory.getLogger(this.getClass);
    def wrn = logger.warn("logtest: warning from class Loo")
}

private class Goo extends Restlet {
    val logger: Logger = LoggerFactory.getLogger(this.getClass);
    def wrn = logger.warn("logtest: warning from class Goo")
}

object TestLogging {

    val logger: Logger = LoggerFactory.getLogger(this.getClass);

    def foo = {
        logger.info("logtest: info log from source function foo")
    }

    def main(args: Array[String]): Unit = {

        println("Logging starting")

        // java.util.logging.manager to org.apache.logging.log4j.jul.LogManager

        System.setProperty("java.util.logging.manager", "org.apache.logging.log4j.jul.LogManager");

        System.setProperty("log4j2.configurationFile", """D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\log4j2.xml""");
        LogStuff.doit();

        logger.trace("logtest: trace log")
        logger.debug("logtest: debug log")
        logger.info("logtest: info log")
        logger.info("logtest: warn log")
        logger.error("logtest: error log")

        Loo.wrn
        Config.validate
        DbSetup.init
        DbSetup.smokeTest
        val foo = new Goo
        foo.wrn

        val web = new WebServer
        println("waiting for web server")

        // Logging.logSevere("This is a severe hello from logging!")
        Thread.sleep(10 * 1000)
        println("Logging is done")
        System.exit(0)
    }

}