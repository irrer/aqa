package org.aqa.webrun.phase2.phase2csv

/**
 * Generate the main index without generating any other docs or CSV files.
 *
 * This does create the ZIP file of all of the CSV files.
 */
object GenerateIndex {
  def main(args: Array[String]): Unit = {
    Phase2Csv.generateIndex()
  }
}
