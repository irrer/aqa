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

import edu.umro.ScalaUtil.FileUtil

import java.io.File

/**
  * Remove old temporary files.
  *
  * It can be useful to keep temporary files for a while (as opposed to deleting
  * them immediately) so that they are available for diagnosing problems.
  */

object TmpCleanup extends Logging {

  /** Hard coded temporary file limit age limit (temporary file retention time). */
  private val maxAge_day = 7.0

  /** Age limit (retention time) in ms. */
  private val maxAge_ms = (maxAge_day * 24 * 60 * 60 * 1000).toLong

  /**
    * Determine if a file is old enough to delete.
    * @param file File to check
    * @return True if old.
    */
  private def isOld(file: File): Boolean = {
    val age_ms = System.currentTimeMillis() - file.lastModified()
    val is = age_ms > maxAge_ms
    is
  }

  /**
    * Remove all old temporary files.  Do this in a Future so that the main process is not impeded.
    */
  def cleanup(): Unit = {
    val start = System.currentTimeMillis()
    logger.info(s"Removing all temporary files older than $maxAge_day days.")
    val toDelete = Util.listDirFiles(Config.tmpDirFile).filter(isOld)
    logger.info(s"Removing ${toDelete.size} temporary files: " + toDelete.map(_.getAbsolutePath).mkString("\n"))
    toDelete.map(FileUtil.deleteFileTree)
    val elapsed = System.currentTimeMillis() - start
    logger.info(s"${toDelete.size} temporary files and directories removed in $elapsed ms.")
  }

}
