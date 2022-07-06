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
    CsvCol("Analysis", "Date and time that data was analyzed.", (o: Output) => Util.standardDateFormat.format(o.startDate).replace('T', ' ')),
    CsvCol("Procedure", "Procedure (test) used to process data", (o: Output) => metadataCache.procedureMap(o.procedurePK).fullName)
  )

  def toCsvText(output: Output): String = {
    colList.map(c => c.toText(output)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
