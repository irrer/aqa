package org.aqa.webrun.phase2.phase2csv

import org.aqa.Util
import org.aqa.db.Output

/**
  * Prefix of each line of CSV that is common across different types of data.
  *
  * @param metadataCache For getting meta data efficiently.
  */
class PrefixCsv(metadataCache: MetadataCache) {

  val colList: Seq[CsvCol[Output]] = Seq(
    CsvCol(
      "Institution",
      "Anonymous version of institution name.",
      (o: Output) => metadataCache.institutionNameMap(metadataCache.machineMap(o.machinePK.get).institutionPK)
    ),
    CsvCol("Machine", "Anonymous version of machine ID.", (o: Output) => metadataCache.machineMap(o.machinePK.get).id),
    CsvCol(
      "Acquisition",
      "Date and time that the first image in the data set was captured (delivery time).",
      (o: Output) => Util.standardDateFormat.format(o.dataDate.get)
    ),
    CsvCol("Analysis", "Date and time that data was analyzed.", (o: Output) => Util.standardDateFormat.format(o.startDate))
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
