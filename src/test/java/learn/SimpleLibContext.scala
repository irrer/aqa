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

import com.typesafe.config._
import edu.umro.ScalaUtil.Trace
import java.io.File

// Whenever you write a library, allow people to supply a Config but
// also default to ConfigFactory.load if they don't supply one.
// Libraries generally have some kind of Context or other object
// where it's convenient to place the configuration.

// we have a constructor allowing the app to provide a custom Config
class SimpleLibContext(config: Config) {

  // This verifies that the Config is sane and has our
  // reference config. Importantly, we specify the "simple-lib"
  // path so we only validate settings that belong to this
  // library. Otherwise, we might throw mistaken errors about
  // settings we know nothing about.
  config.checkValid(ConfigFactory.defaultReference(), "simple-lib")

  // This uses the standard default Config, if none is provided,
  // which simplifies apps willing to use the defaults
  def this() {
    this(ConfigFactory.load())
  }

  // this is the amazing functionality provided by simple-lib
  def printSetting(path: String) {
    println("The setting '" + path + "' is: " + config.getString(path))
  }
}

// Here is an OPTIONAL alternative way to access settings, which
// has the advantage of validating fields on startup and avoiding
// typos. This is redundant with the SimpleLibContext above,
// in fact we'll show a settings-based context below.
class SimpleLibSettings(config: Config) {

  // checkValid(), just as in the plain SimpleLibContext
  config.checkValid(ConfigFactory.defaultReference(), "simple-lib")

  // note that these fields are NOT lazy, because if we're going to
  // get any exceptions, we want to get them on startup.
  val foo: String = config.getString("simple-lib.foo")
  val hello: String = config.getString("simple-lib.hello")
  val whatever: String = config.getString("simple-lib.whatever")
}

// This is a different way to do SimpleLibContext, using the
// SimpleLibSettings class to encapsulate and validate your
// settings on startup
class SimpleLibContext2(config: Config) {
  val settings = new SimpleLibSettings(config)

  def this() {
    this(ConfigFactory.load())
  }

  // this is the amazing functionality provided by simple-lib with a Settings class
  def printSettings() {
    println("foo=" + settings.foo)
    println("hello=" + settings.hello)
    println("whatever=" + settings.whatever)
  }
}

object SimpleLibContextMain {
  def main(args: Array[String]): Unit = {
    Trace.trace("Starting")
    val file = new File("reference.conf")
    val config = ConfigFactory.parseFile(file)
    val foo: String = config.getString("simple-lib.foo")
    Trace.trace("foo: " + foo)
    val stuff = com.typesafe.config.ConfigFactory.load(config)
    //Trace.trace(stuff.toString.split(",").mkString("\n"))
    Trace.trace("Done")
  }
}


