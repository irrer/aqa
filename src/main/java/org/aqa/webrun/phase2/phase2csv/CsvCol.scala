package org.aqa.webrun.phase2.phase2csv

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

  /**
   * Provide documentation of this column.
   */
  val doc = {
    <tr><td>{header}</td><td>{description}</td></tr>
  }
}
