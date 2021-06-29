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


import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

object ParIt {

  def main(args: Array[String]): Unit = {

    val rand = new Random

    def rr(i: Int): Int = {
      val ms = rand.nextInt(1000)
      println("start  i: " + i + "    ms: " + ms)
      Thread.sleep(ms)
      println("finish i: " + i + "    ms: " + ms)
      i
    }

    val seq = Seq(() => rr(1), () => rr(2), () => rr(3), () => rr(4), () => rr(5))

    val text = seq.par.map(f => f()).toSeq.mkString("  ")
    println("text: " + text)

  }

}