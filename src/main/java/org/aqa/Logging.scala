package org.aqa

import java.util.logging.Level
import java.util.logging.FileHandler
import java.util.logging.Logger
import edu.umro.util.Log
import org.restlet.Restlet
import org.restlet.routing.Filter

object Logging {
    private def log(level: Level, text: String) = {
        Log.get.log(level, text)
        //println(level.getName + "|" + text) // TODO write to actual logger
    }

    /**
     * Format a <code>Throwable</code>.
     *
     * @param throwable Contains description and stack trace.
     *
     * @return Human readable version of <code>Throwable</code> and stack trace.
     */
    def fmtEx(throwable: Throwable): String = {
        val textList = throwable.getStackTrace.map(ste => "\n    " + ste) // convert to text
        textList.foldLeft(throwable.toString)((t, ste) => t + ste) // join as one string
    }

    def logFinest(msg: String) = log(Level.FINEST, msg)

    def logFiner(msg: String) = log(Level.FINER, msg)

    def logFine(msg: String) = log(Level.FINE, msg)

    def logInfo(msg: String) = log(Level.INFO, msg)

    def logWarning(msg: String) = log(Level.WARNING, msg)

    def logSevere(msg: String) = log(Level.SEVERE, msg)

    def main(args: Array[String]): Unit = {
        println("Logging starting")
        //System.setProperty("java.util.logging.config.file", """D:\pf\eclipse\workspaceMars\aqa\log4j.properties""")
        //System.setProperty("java.util.logging.config.file", """D:\pf\eclipse\workspaceMars\aqa\src\resources\logging.propertiesWindows""")
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
        import org.apache.log4j.FileAppender
        val fileAppender = new FileAppender

        //org.apache.commons.logging.impl.SimpleLog.warn()

        // println("simple.dateTimeFormat: " + simple.)

        logSevere("Hello from logging!")
        println("Logging done")
        System.exit(99)
    }
        
}