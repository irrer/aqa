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
  * Define whether data should be considered valid or otherwise.
  */
object DataValidity extends Enumeration {

  val valid: DataValidity.Value = Value // good value
  val invalid: DataValidity.Value = Value // bad value.  Possible error in setup or calculation.

  /**
    * Convert text to a Validity.  Is case insensitive.
    */
  def stringToDataValidity(text: String): Option[DataValidity.Value] = DataValidity.values.find(ur => ur.toString.equalsIgnoreCase(text))

  /**
    * Convert text to a Validity.  If the string from the database does not match any
    * known state, then assume the default state.  Is case insensitive.
    */
  def stringToDataValidityWithDefault(text: String, defaultState: DataValidity.Value = valid): DataValidity.Value = {
    DataValidity.values.find(ur => ur.toString.equalsIgnoreCase(text)) match {
      case Some(state) => state
      case _           => defaultState
    }
  }
}
