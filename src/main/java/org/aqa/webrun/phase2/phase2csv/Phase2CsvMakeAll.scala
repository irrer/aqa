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

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DbSetup

/**
  * Generate all CSV files and documentation files.
  */
object Phase2CsvMakeAll extends Logging {

  /**
    * Make all of the CSV content, overwriting old content.
    */
  def makeAll(): Unit = {

    val start = System.currentTimeMillis()
    logger.info("Starting CSV generation.")

    val metadataCache = MetadataCache.metadataCache

    (new PopulateDicomCsv).populateAll() // Get the DICOM column data up to date.

    val dataTypeList = Seq(
      new CenterDoseCsv,
      new CollimatorCenteringCsv,
      new CollimatorPositionCsv,
      new LeafPositionCsv,
      new MetadataCheckCsv,
      new SymmetryAndFlatnessCsv,
      new VMAT_T2_DR_GSCsv,
      new VMAT_T2_DG_RSCsv,
      new VMAT_T3MLCSpeedCsv,
      new WedgePointCsv
    )

    for (institutionPK <- metadataCache.institutionNameMap.keys) {
      val machineList = metadataCache.machineMap.values.filter(_.institutionPK == institutionPK).toSeq.sortBy(_.id.toUpperCase())
      val csvDir = Phase2Csv.institutionCsvDir(institutionPK)
      for (dt <- dataTypeList) {
        try {
          dt.writeToFile(csvDir, machineList)
        } catch {
          case t: Throwable => logger.error("Unexpected exception writing " + dt.getDataName + " : " + fmtEx(t))
        }
      }
      MaintenanceCsv.writeToFile(csvDir, machineList)
    }

    // write the documentation for each type of data
    dataTypeList.foreach(dt => dt.writeDoc())

    MaintenanceCsv.writeDoc()

    // Write an index for each institution, regardless of whether or not it has data.
    metadataCache.institutionNameMap.keys.foreach(institutionPK => Phase2Csv.generateIndex(institutionPK))

    logger.info("Done with CSV generation.  Elapsed time: " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }

  /**
    * 'main' wrapper for function.
    * @param args Not used.
    */
  def main(args: Array[String]): Unit = {
    logger.info("Initializing database for CSV generation ...")
    DbSetup.init
    makeAll()
    Thread.sleep(1000)
    System.exit(0) // Note:
  }

}
