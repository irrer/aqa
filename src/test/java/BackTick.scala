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

object BackTick {

  private class Thingy(id: Int) {
    val v = "me: " + id
    println("v: " + v)
  }

  def main(args: Array[String]): Unit = {

    lazy val lzy = new Thingy(5)
    lazy val j1 = new Thingy(1)
    lazy val j2 = new Thingy(2)
    lazy val j3 = new Thingy(3)

    j1 match {
      case `j2` => println("got j2")
      case `lzy` => println("got lzy")
      case `j3` => println("got j3")
      case `j1` => println("got j1")
      case _ => println("nuthin")
    }

  }

}