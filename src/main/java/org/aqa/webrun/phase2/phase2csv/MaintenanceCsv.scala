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
import org.aqa.db.Machine
import org.aqa.db.MaintenanceRecord

import java.io.File

/**
  * Support both the creation of a maintenance record CSV file and the insertion of
  * maintenance records within another CSV file.
  */

class MaintenanceCsv(metadataCache: MetadataCache) {

  private val dataName = "Maintenance Record"
  // private val fileBaseName = Phase2Csv.fil

  /**
    * Replace newline characters with blanks so they only occupy one line of the CSV file.
    *
    * Processes both \n and \r.
    *
    * @param text Might contain newlines.
    * @return Text with newlines replaced by blanks.
    */
  private def replaceNewline(text: String): String = {
    text.replace('\n', ' ').replace('\r', ' ')
  }

  private val colList = Seq(
    CsvCol(
      "Institution",
      "Anonymous version of institution name.",
      (mt: MaintenanceRecord) => metadataCache.institutionNameMap(metadataCache.machineMap(mt.machinePK).institutionPK)
    ),
    CsvCol("Machine", "Anonymous version of machine id.", (mt: MaintenanceRecord) => metadataCache.machineMap(mt.machinePK).id),
    CsvCol("Effective Date", "Date and time of maintenance.", (mt: MaintenanceRecord) => Util.standardDateFormat.format(mt.creationTime)),
    CsvCol(
      "Maintenance Record Marker",
      "Marks this row as a maintenance record.  This makes finding them straightforward when they are embedded in a CSV file.",
      (_: MaintenanceRecord) => "Maintenance Record"
    ),
    CsvCol("Maintenance Category", "Type of maintenance.", (mt: MaintenanceRecord) => mt.category),
    CsvCol("Created by User", "Anonymous version of user id.", (mt: MaintenanceRecord) => metadataCache.userMap(mt.userPK)),
    CsvCol("Summary", "Short description of maintenance.", (mt: MaintenanceRecord) => replaceNewline(mt.summary)),
    CsvCol("Description", "Full description of maintenance.", (mt: MaintenanceRecord) => replaceNewline(mt.description))
  )

  /** The CSV headers for maintenance records. */
  private val headerText: String = colList.map(c => c.header).mkString(",")

  /**
    * Convert a maintenance record into a single CSV row
    * @param maintenanceRecord From database to convert
    * @return A single line of text
    */

  def toCsvText(maintenanceRecord: MaintenanceRecord): String = {
    colList.map(mtCsv => mtCsv.toText(maintenanceRecord)).mkString(",")
  }

  private def makeCsv(machine: Machine): String = {
    val mrList = MaintenanceRecord.getByMachine(machine.machinePK.get)
    if (mrList.isEmpty)
      ""
    else {
      mrList.map(mr => toCsvText(mr)).mkString("\n")
    }
  }

  /**
    * Create the MaintenanceRecord.csv file for all institutions.
    */
  def writeToFile(csvDir: File, machList: Seq[Machine]): Unit = {
    // list of all machines with institutions grouped together
    val machineList = machList.sortBy(_.institutionPK)

    val rows = machineList.map(m => makeCsv(m))

    val fullText = headerText + "\n" + rows.filter(_.nonEmpty).mkString("\n\n")

    Phase2Csv.writeToFile(csvDir, fullText, dataName)
  }

  /**
    * Write the documentation for columns.
    */
  def writeDoc(): Unit = {
    Phase2Csv.writeDoc(colList.asInstanceOf[Seq[CsvCol[Any]]], dataName)
  }

}
