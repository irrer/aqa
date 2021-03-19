package org.aqa.webrun.phase2.phase2csv

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Output

class PrefixCsv(metadataCache: MetadataCache) {

  private case class PrefixCol(header: String, toText: Output => String) {}

  private val colList = Seq(
    PrefixCol("Institution", (o: Output) => metadataCache.institutionNameMap(metadataCache.machineMap(o.machinePK.get).institutionPK)),
    PrefixCol("Machine", (o: Output) => metadataCache.machineMap(o.machinePK.get).id),
    PrefixCol("Acquisition", (o: Output) => Util.standardDateFormat.format(o.dataDate.get)),
    PrefixCol("Analysis", (o: Output) => Util.standardDateFormat.format(o.startDate)),
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
