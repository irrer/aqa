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

package learn

/**
 * Failed experiment.  Could not get this to catch an out of memory/java heap space error.
 */

object UncaughtEx {

  def grabMem = {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.util.{ Failure, Success }
    val grabby = Future {
      try {
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler(2))
        Thread.sleep(1 * 1000)
        println("========================== getting grabby ==========================")

        val mem = scala.collection.mutable.ArrayBuffer[Int](2, 3, 5, 7, 11, 13, 17, 19)
        while (true) {
          Thread.sleep(20)
          println("mem.size: " + mem.size)
          mem ++= mem.take(mem.size / 3)
        }
      } catch {
        case t: Throwable => println("grabby caught: " + t)
      }
    }
  }

  /**
   * If there is an out of memory error then restart the service.
   */
  private class UncaughtExceptionHandler(id: Int) extends Thread.UncaughtExceptionHandler {
    override def uncaughtException(t: Thread, e: Throwable) = {
      // Use the simplest printing of information as possible.  More sophisticated
      // methods may fail because they might try to get more memory.
      println("id : " + id + " unexpected error in UncaughtExceptionHandler : " + e)
      e.printStackTrace
      if (e.isInstanceOf[OutOfMemoryError]) {
        println("Whoa!")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler(1))
      grabMem

      Thread.sleep(20 * 60 * 1000)
      println("Exiting")
      System.exit(0)
    } catch {
      case t: Throwable => println("main caught: " + t)
    }
  }

}
