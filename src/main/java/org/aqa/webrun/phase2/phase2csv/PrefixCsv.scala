package org.aqa.webrun.phase2.phase2csv

import org.aqa.Util
import org.aqa.db.Output

import java.text.SimpleDateFormat

/**
  * Prefix of each line of CSV that is common across different types of data.
  *
  * @param metadataCache For getting meta data efficiently.
  */
class PrefixCsv(metadataCache: MetadataCache) {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  val timeFormat = new SimpleDateFormat("HH:mm:ss")

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
      (o: Output) => Util.standardDateFormat.format(o.dataDate.get).replace('T', ' ')
    ),
    CsvCol(
      "Acquisition Date",
      "Date that the first image in the data set was captured (delivery date).",
      (o: Output) => dateFormat.format(o.dataDate.get)
    ),
    CsvCol(
      "Acquisition Time",
      "Time of day that the first image in the data set was captured (delivery time).",
      (o: Output) => timeFormat.format(o.dataDate.get)
    ),
    CsvCol("Analysis", "Date and time that data was analyzed.", (o: Output) => Util.standardDateFormat.format(o.startDate).replace('T', ' '))
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
