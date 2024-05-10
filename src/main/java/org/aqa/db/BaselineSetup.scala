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

package org.aqa.db

/**
  * How the baseline was created.
  */
object BaselineSetup extends Enumeration {

  /** When a baseline value is needed, but none has been established, a procedure may default to the values from the given data set.*/
  val byDefault: BaselineSetup.Value = Value

  /** When a user explicitly decides that a data value is to be used as a baseline. */
  val chosen: BaselineSetup.Value = Value

  /**
    * Given a string get the corresponding <code>BaselineSetup</code>.
    */
  def stringToBaselineSetup(name: String): Option[BaselineSetup.Value] = {
    val matches = BaselineSetup.values.filter(s => name.equalsIgnoreCase(s.toString)).toList
    matches.headOption
  }
}
