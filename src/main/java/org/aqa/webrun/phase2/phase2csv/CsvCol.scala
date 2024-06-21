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

package org.aqa.webrun.phase2.phase2csv

import scala.xml.Elem

/**
  * Define a column in a Phase2 CSV.
  *
  * @param header Name of column.
  * @param description: Describes to a user what the contents of the column
  *                     represent.  Used for generating documentation.
  * @param toText Converts given data to text that will be put in the cell.
  */
case class CsvCol[T](header: String, description: String = "", toText: T => Any, isBeamName: Boolean = false) {
  // def this(header: String, toText: T => Any) = this(header, "", toText)

  private val alphabet = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

  def indexToAlpha(index: Int): String = {
    val hi = {
      if (index > 25) alphabet(index / 26 - 1) else ""
    }
    val lo = alphabet(index % 26)
    hi + lo
  }

  /**
    * Provide documentation of this column.
    */
  def doc(index: Int): Elem = {
    <tr>
      <td>{indexToAlpha(index)}</td>
      <td>{header}</td>
      <td>{description}</td>
    </tr>
  }
}
