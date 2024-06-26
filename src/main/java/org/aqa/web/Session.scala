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

package org.aqa.web

import org.aqa.Config

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

/**
  * Provide a session id that will be considered to be unique.  Designed to
  * be used for user sessions to keep them distinct from each other.  The
  * current time is used both for uniqueness and as a debugging/diagnostic
  * aid.
  */

object Session {

  /**
    * Make a unique session id.
    */
  def makeUniqueId: String =
    Session.synchronized({
      val sessionFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")
      val start = System.currentTimeMillis
      while (System.currentTimeMillis == start) Thread.sleep(1)
      val s = sessionFormat.format(new Date)
      s
    })

  def idToFile(id: String): File = new File(Config.tmpDirFile, id)

  def main(args: Array[String]): Unit = {
    for (i <- (0 until 5)) println(makeUniqueId)
  }
}
