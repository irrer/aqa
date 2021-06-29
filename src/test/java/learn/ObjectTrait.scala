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

import edu.umro.ScalaUtil.Trace

object ObjectTrait {

  trait Trate {
    def defA = Trace.trace("defA")
    def defB = Trace.trace("defB")
    val valA = "valA"
    val valB = "vaB"

    def def1: String;
    def def2: String;
    val val1: String;
    val val2: String;
  }

  object Obj1 extends Trate {
    override def def1 = "def1"
    def def2 = "def2"
    override val val1 = "val1"
    val val2 = "val2"
  }

  object Runner {
    def runIt(r: Trate) = {
      Trace.trace(r.def1)
      Trace.trace(r.def2)
      Trace.trace(r.valA)
    }
  }

  def main(args: Array[String]): Unit = {
    Runner.runIt(Obj1)
  }

}