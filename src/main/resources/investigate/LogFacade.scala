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

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import org.apache.commons.logging.Log
import org.apache.commons.logging.LogFactory

object LogFacade {

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

    /*
    private def log(level: Level, text: String) = {
        Log.get.log(level, text)
        //println(level.getName + "|" + text) // TODO write to actual logger
    }

    def logFinest(msg: String) = log(Level.FINEST, msg)

    def logFiner(msg: String) = log(Level.FINER, msg)

    def logFine(msg: String) = log(Level.FINE, msg)

    def logInfo(msg: String) = log(Level.INFO, msg)

    def logWarning(msg: String) = log(Level.WARNING, msg)

    def logSevere(msg: String) = log(Level.SEVERE, msg)
    */

    def main(args: Array[String]): Unit = {
        println("LogFacade starting")
        val logProp = System.setProperty("org.apache.commons.logging.Log", "org.apache.commons.logging.impl.SimpleLog")
        println("logProp prev value: " + logProp)
        val logger = LoggerFactory.getLogger(LogFacade.getClass)
        logger.info("Hello World")
        println("LogFacade done")
        System.exit(99)
    }
}