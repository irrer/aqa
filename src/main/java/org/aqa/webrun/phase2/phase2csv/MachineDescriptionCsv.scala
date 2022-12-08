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
    CsvCol[Output]("outputPK", "Internal database reference to synthetic public key of output.", (o: Output) => o.outputPK.get),
    CsvCol[Output]("inputPK", "Internal database reference to synthetic public key of input.", (o: Output) => o.inputPK),
    CsvCol[Output]("Uploaded By", "Anonymized version of user ID.", (o: Output) => metadataCache.userMap(o.userPK.get))
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
