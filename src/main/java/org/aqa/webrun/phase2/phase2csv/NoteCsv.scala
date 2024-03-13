package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.Output

/**
  * Support generating CSV for user defined output notes.
  * @param metadataCache Cache of all notes.
  */
class NoteCsv(metadataCache: MetadataCache) {

  private def getNote(output: Output): String = {
    if (metadataCache.noteMap.contains(output.outputPK.get)) metadataCache.noteMap(output.outputPK.get) else ""
  }

  /** List of columns.  */
  val colList: Seq[CsvCol[Output]] = Seq(
    CsvCol("Note", "User defined note for this output.", getNote)
  )

  /**
    * Convert an output to one line of note for CSV.
    * @param output For this output.
    * @return Note text.  If not defined, then it will be an empty string.
    */
  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
