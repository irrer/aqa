package org.aqa.webrun.phase2.phase2csv

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Output

class MachineDescriptionCsv(metadataCache: MetadataCache) {

  private case class PrefixCol(header: String, toText: Output => String) {}

  private val colList = Seq(
    PrefixCol("Type", (o: Output) => metadataCache.machineTypeMap(metadataCache.machineMap(o.machinePK.get).machineTypePK)),
    PrefixCol("Collimator", (o: Output) => metadataCache.collimatorTypeMap(metadataCache.machineMap(o.machinePK.get).multileafCollimatorPK)),
    PrefixCol("EPID", (o: Output) => metadataCache.epidTypeMap(metadataCache.machineMap(o.machinePK.get).epidPK)),
    PrefixCol("URL", (o: Output) => Config.RootUrl + "/view/ViewOutput?outputPK=" + o.outputPK.get),
    PrefixCol("Uploaded By", (o: Output) => metadataCache.userMap(o.userPK.get))
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
