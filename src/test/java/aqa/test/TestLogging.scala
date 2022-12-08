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


package aqa.test;

import scala.Enumeration
import java.io.File
import org.restlet.Restlet
//import org.slf4j.Logger
//import org.slf4j.LoggerFactory
import play.api.Logger
//import com.typesafe.scalalogging._
import org.aqa.db.DbSetup
import org.aqa.Config
import org.aqa.web.WebServer
import org.scalatest.FlatSpec
import org.scalatest.Matchers
//import java.io.File

/**
 * Test the logging infrastructure.
 *
 * Note that to enable log4j2, the following VM argument must be set when invoking the java run time.  It does not work to set the property
 *
 *    -Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager
 *    -Dlog4j2.configurationFile=src\test\resources\log4j2.xml
 *
 */

//class LogObject {
//    private val logger = Logger(this.getClass)
//}
//
//object LogObject {
//    val logger = (new LogObject).logger
//}
//
//private object OtherClass {
//    val logger: Logger = Logger(this.getClass)
//    def wrn = logger.warn("logtest: warning from class Loo")
//}

class TestLogging extends FlatSpec with Matchers {
    private val logger = org.slf4j.LoggerFactory.getLogger("")

//    System.getProperty("java.util.logging.manager") should not be (null)
//
//    verifyProperty("java.util.logging.manager")
//    verifyProperty("log4j2.configurationFile")
//
//    System.getProperty("log4j2.configurationFile") should not be (null)
//
//    private def verifyProperty(name: String) = {
//        System.getProperty(name) should not be (null)
//        println("System Property " + name + " : " + System.getProperty(name))
//    }
//
//    println("logger: " + logger)
//
//    private class InnerPrivateClass {
//        logger.info("logtest: info log from InnerPrivateClass")
//    }
//
//    private class InnerPublicClass {
//        logger.info("logtest: info log from InnerPublicClass")
//        //LogObject.logger.info("logtest: info log from InnerPublicClass with LogObject.logger")
//    }
//
//    private def foo = {
//        logger.info("logtest: info log from function foo")
//    }

    private def performLogging = {

        // java.util.logging.manager to org.apache.logging.log4j.jul.LogManager

        // Nope; does not work.
        //System.setProperty("java.util.logging.manager", "org.apache.logging.log4j.jul.LogManager")

        //System.setProperty("log4j2.configurationFile", """D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\log4j2.xml""")
//        LogStuff.doit();

        logger.trace("logtest: trace log")
        logger.debug("logtest: debug log")
        logger.info("logtest: info log")
        logger.info("logtest: warn log")
        logger.error("logtest: error log")

        //OtherClass.wrn
//        Config.validate
//        DbSetup.init
//        DbSetup.smokeTest
//
//        new InnerPrivateClass
//
//        new InnerPublicClass
//
//        // val web = new WebServer
//        println("waiting for web server")
//
//        // Logging.logSevere("This is a severe hello from logging!")
//        //Thread.sleep(10 * 1000)
//        println("Logging is done")
//        //System.exit(0)
    }

    "logging" should "write messages to a file" in {
        performLogging
    }

    println("Logging class is done")
}

//object TestLogging {
//    def main(args: Array[String]): Unit = {
//        val tl = new TestLogging
//        tl.performLogging
//    }
//}

//import collection.mutable.Stack
//
//class ExampleSpec extends FlatSpec with Matchers {
//
//    "A Stack" should "pop values in last-in-first-out order" in {
//        val stack = new Stack[Int]
//        stack.push(1)
//        stack.push(2)
//        stack.pop() should be(2)
//        stack.pop() should be(1)
//    }
//
//    it should "throw NoSuchElementException if an empty stack is popped" in {
//        val emptyStack = new Stack[Int]
//        a[NoSuchElementException] should be thrownBy {
//            emptyStack.pop()
//        }
//    }
//}

