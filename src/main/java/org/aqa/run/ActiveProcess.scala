package org.aqa.run

import org.aqa.db.Output
import scala.collection.mutable.HashMap
import scala.sys.process._

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

class ActiveProcess(val output: Output, val process: Process, val logger: StdLogger);
