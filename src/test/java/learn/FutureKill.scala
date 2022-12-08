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

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import edu.umro.ScalaUtil.Trace

object FutureKill {

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    def elapsed: String = (System.currentTimeMillis - start).formatted("%8d    ")

    val forever = Future {
      while (true) {
        println(elapsed + "I am forever ")
        Thread.sleep(1000)
        println(elapsed + "forever done")
      }
    }

    class WaitForIt extends Runnable {
      override def run = {
        while (true) {
          println(elapsed + "forever.isCompleted: " + forever.isCompleted + "    ")
          Thread.sleep(333)
        }
      }
      (new Thread(this)).start
    }

    Trace.trace
    new WaitForIt
    Trace.trace

    val timeout = Duration(1500, TimeUnit.MILLISECONDS)
    Trace.trace

    try {
      Await.result(forever, timeout)
    } catch {
      case t: Throwable => println(elapsed + "got exception: " + t)
    }
    Trace.trace

    Thread.sleep(5000)
    Trace.trace
    println(elapsed + "exiting")
    System.exit(0)
  }

}
