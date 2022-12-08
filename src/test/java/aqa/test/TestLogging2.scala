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

package aqa.test

trait LogIt {
    val logger = org.slf4j.LoggerFactory.getLogger("")
    implicit def logit(any: Any) = {
        val msg = any.toString
        logger.info(msg)
    }
}

//private class TestLogging2 extends LogIt {
private class TestLogging2 {
    //import LogMe._

    val logger = LogLog.logger
    val logIt: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger("")
    val hi = "hi from TestLogging2"
    logIt.info(hi)
    //logit(hi)

    def func = {
        val msg = "from logme from func"
        logger.info(msg)
        //        logger.info(???, "")
        logIt.info("logIt in func")
    }

    class Inside {
        logger.info("Inside")
        logIt.info("logIt in Inside")
        def funcy = {
            logIt.info("logIt funcy in Inside")
        }
        funcy
    }

    class Inside2 extends LogIt {
        logger.info("Inside2")
        logIt.info("logIt in Inside2")
        def funcy = {
            logIt.info("logIt funcy in Inside2")
        }
        funcy
    }

    func

    (new Inside).funcy
    (new Inside2).funcy

    println("exiting PlayClass")
}

object Thingy extends LogIt {
    def dolog = {
        logger.info("dolog is talking")
    }
}

object TestLogging2 {
    def main(args: Array[String]): Unit = {
        println("starting")
        import java.io.File

        println("System Property " + "java.util.logging.manager" + " = " + System.getProperty("java.util.logging.manager"))
        println("System Property " + "log4j2.configurationFile" + " = " + System.getProperty("log4j2.configurationFile"))

        (new TestLogging2)
        Thingy.dolog
    }

}
