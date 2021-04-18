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
case class CsvCol[T](header: String, description: String = "", toText: T => Any) {
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
