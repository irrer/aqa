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

object Angles {

  def main(args: Array[String]): Unit = {

    def show(deg: Double) = {
      val rad = Math.toRadians(deg)
      val s = (Math.sin(rad) * 1000).round / 1000.0
      val c = (Math.cos(rad) * 1000).round / 1000.0
      println(deg.formatted("%8.1f") + "    rad: " + rad.formatted("%6.3f") + "    sin: " + s.formatted("%6.3f") + "    cos: " + c.formatted("%6.3f"))
    }

    for (deg <- -360 until 420 by 30) show(deg)

  }

}