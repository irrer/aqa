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

import com.typesafe.config.ConfigFactory
import edu.umro.ScalaUtil.Trace
import java.io.File

object SimpleLibContextMin {
  def main(args: Array[String]): Unit = {
    Trace.trace("Starting")
    val file = new File("reference.conf")
    val config = ConfigFactory.parseFile(file)
    val foo: String = config.getString("simple-lib.foo")
    Trace.trace("foo: " + foo)
    //val stuff = com.typesafe.config.ConfigFactory.load(config)
    //Trace.trace(stuff.toString.split(",").mkString("\n"))
    Trace.trace("Done")
  }
}


