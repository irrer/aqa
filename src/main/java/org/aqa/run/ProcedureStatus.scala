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

//import collection.JavaConverters._
import org.aqa.run
import resource.managed

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import scala.collection.mutable.ArrayBuffer

private object ProcedureStatusList {
  private val statusList = ArrayBuffer[ProcedureStatus.ProcedureStatus]()

  def add(status: ProcedureStatus.ProcedureStatus): ArrayBuffer[ProcedureStatus.ProcedureStatus] = statusList += status

  def list: List[ProcedureStatus.ProcedureStatus] = statusList.toList
}

/**
  * Various types of procedure termination statuses.  Add more as needed.
  */
object ProcedureStatus extends Enumeration {

  /** When storing a ProcedureStatus in a file, the file must have this name. */
  val statusFileName = "status.txt"

  val running: ProcedureStatus = Value("running", "Procedure is currently running.")
  val pass: ProcedureStatus = Value("pass", "All results are within acceptable limits.")
  val fail: ProcedureStatus = Value("fail", "At least one result was outside acceptable limits.")
  val done: ProcedureStatus = Value("done", "Completed, but no indication as to whether results are acceptable or not.")
  val abort: ProcedureStatus = Value("abort", "Prematurely terminated itself.")
  val dberr: ProcedureStatus = Value("dberr", "Problem using the database from the procedure.")
  val userabort: ProcedureStatus = Value("userabort", "Prematurely terminated by user.")
  val timeout: ProcedureStatus = Value("timeout", "Terminated when time limit and was exceeded.")
  val crash: ProcedureStatus = Value("crash", "Terminated itself in an uncontrolled fashion.")
  val servershutdown: ProcedureStatus = Value("servershutdown", "Server was shut down while the procedure was running.")
  val warning: ProcedureStatus = Value("warning", "Results were acceptable but nearly failed.")

  class ProcedureStatus(val name: String, val description: String) extends Val(nextId, name) {}

  protected final def Value(name: String, description: String): ProcedureStatus = {
    val status = new ProcedureStatus(name, description)
    ProcedureStatusList.add(status)
    status
  }

  private val maxNameLength = ProcedureStatus.values.map(s => s.toString.length).max

  def stringToProcedureStatus(name: String): Option[ProcedureStatus.Value] = {
    val matches = ProcedureStatus.values.filter(s => name.equalsIgnoreCase(s.toString)).toList
    matches.headOption
  }

  def descriptionOf(status: ProcedureStatus.Value): String = {
    val name = status.toString
    ProcedureStatusList.list.filter(s => s.name.equals(name)).head.description
  }

  /**
    * Given a file, try to read a status (by name) from it.
    */
  def fileToProcedureStatus(file: File): Option[ProcedureStatus.Value] = {
    try {
      val buf = Array.ofDim[Byte](maxNameLength)
      managed(new FileInputStream(file)) acquireAndGet { fis =>
        {
          val size = fis.read(buf)
          val name = new String(buf).substring(0, size).toLowerCase.trim
          stringToProcedureStatus(name)
        }
      }
    } catch {
      case _: Throwable => None
    }
  }

  def dirToProcedureStatus(dir: File): Option[ProcedureStatus.Value] = ProcedureStatus.fileToProcedureStatus(new File(dir, ProcedureStatus.statusFileName))

  def writeProcedureStatus(dir: File, status: ProcedureStatus.Value): Unit = {
    try {
      val statusFile = new File(dir, statusFileName)
      managed(new FileOutputStream(statusFile)) acquireAndGet { fos =>
        {
          fos.write(status.toString.getBytes)
        }
      }
    } catch {
      case _: Throwable => ;
    }
  }

  /**
    * Convenience function to log a message to stdout, write a status file, and
    *  exit.  The exit code will be 0 for a status of 'pass' or 'done', and 1 for
    *  all other status values.
    */
  def terminate(msg: String, status: ProcedureStatus.Value): Unit = {
    println(msg)
    ProcedureStatus.writeProcedureStatus(status)
    val exitCode: Int = status match {
      case ProcedureStatus.pass => 0
      case ProcedureStatus.done => 0
      case _                    => 1
    }
    System.exit(exitCode)
  }

  def writeProcedureStatus(status: ProcedureStatus.Value): Unit = writeProcedureStatus(new File("."), status)

  /**
    * Sort by id.
    */
  def sort(seq: Seq[ProcedureStatus.Value]): Seq[run.ProcedureStatus.Value] = seq.sortWith((a, b) => a.id < b.id)

  def eq(a: Value, b: Value): Boolean = a.toString.equals(b.toString)

  /**
    * For internal testing only.
    */
  def main(args: Array[String]): Unit = {

    val first = ProcedureStatus.values.toArray.toList.head

    if (true) {
      println("starting")
      val s = stringToProcedureStatus("timeout")
      println("s: " + s)
      System.exit(99)
    }

    if (true) {
      println("cwd: " + new File(".").getAbsolutePath)
      println("status.good: " + fileToProcedureStatus(new File("status.good")))
      println("status.bad: " + fileToProcedureStatus(new File("status.bad")))
      System.exit(99)
    }

    if (true) {
      val lst = List(abort, fail, done, pass)
      println("lst: " + lst)
      val srt = sort(lst)
      println("srt: " + srt)
      System.exit(99)
    }

    println("first: " + first)

    def show(s: ProcedureStatus.ProcedureStatus): Unit = {
      println
      println("s: " + s)
      println("s.toString: " + s.toString)
      println("s.description: " + s.description)
      println("s.asInstanceOf[ProcedureStatus.ProcedureStatus].description: " + s.description)
      println("s.+(\"hey\"): " + s.+("hey"))
      println("s.id: " + s.id)
    }
    println
    println("ProcedureStatus.maxId: " + ProcedureStatus.maxId)

    val p = ProcedureStatus.pass
    show(p)
    show(ProcedureStatus.abort)

    val newProcedureStatus = ProcedureStatus.Value("hey", "Well hello there")
    show(newProcedureStatus)

    println
    println("p.description: " + p.description)
  }
}
