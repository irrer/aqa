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

package org.aqa.run

import org.aqa.db.Output
import scala.collection.mutable.HashMap
import scala.sys.process._
import org.restlet.Response

/**
 * Keep track of running processes.
 *
 * Although the database can be queried for running processes, it is not possible,
 * given only that information, to destroy a process or close a logger.
 */
object ActiveProcess {

  private val activeProcessList = new HashMap[Long, ActiveProcess]()

  def add(proc: ActiveProcess): Unit = activeProcessList.synchronized({ activeProcessList.put(proc.output.outputPK.get, proc) })

  def get(outputPK: Long) = activeProcessList.synchronized({ activeProcessList.get(outputPK) })

  def remove(outputPK: Long) = activeProcessList.synchronized({ activeProcessList.remove(outputPK) })

  def list = activeProcessList.synchronized({ activeProcessList.map(ap => ap._2) })
}

class ActiveProcess(val output: Output, val process: Process, val postProcess: Option[PostProcess], val logger: StdLogger, val response: Response);