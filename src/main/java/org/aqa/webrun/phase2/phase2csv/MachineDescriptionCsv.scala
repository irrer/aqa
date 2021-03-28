package org.aqa.webrun.phase2.phase2csv

import org.aqa.Config
import org.aqa.db.Output
import org.aqa.web.WebServer

class MachineDescriptionCsv(metadataCache: MetadataCache) {

  private def urlOfOutput(output: Output): String = {
    Config.RootUrl + WebServer.urlOfResultsFile(output.dir) + "/" + Output.displayFilePrefix + ".html"
  }

  val colList = Seq(
    CsvCol[Output]("Machine Type", "Type of machine", (o: Output) => metadataCache.machineTypeMap(metadataCache.machineMap(o.machinePK.get).machineTypePK)),
    CsvCol[Output]("Collimator", "Type of collimator", (o: Output) => metadataCache.collimatorTypeMap(metadataCache.machineMap(o.machinePK.get).multileafCollimatorPK)),
    CsvCol[Output]("EPID", "Type of EPID", (o: Output) => metadataCache.epidTypeMap(metadataCache.machineMap(o.machinePK.get).epidPK)),
    CsvCol[Output]("URL", "Link to main report.", (o: Output) => urlOfOutput(o)),
    CsvCol[Output]("Uploaded By", "Anonymized version of user ID.", (o: Output) => metadataCache.userMap(o.userPK.get))
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
