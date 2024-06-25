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
  private def makeAll(): Unit = {

    val start = System.currentTimeMillis()
    logger.info("Starting CSV generation.")

    val metadataCache = new MetadataCache
    val dataTypeList = Phase2Csv.makeDataTypeList(metadataCache)

    new PopulateDicomCsv(metadataCache).populateAll() // Get the DICOM column data up to date.
    val maintenanceCsv = new MaintenanceCsv(metadataCache)
    for (institutionPK <- metadataCache.institutionNameMap.keys) {
      val machineList = {
        val s = metadataCache.machineMap.values.filter(_.institutionPK == institutionPK).toSeq.sortBy(_.id.toUpperCase())
        scala.collection.immutable.Seq(s).flatten
      }
      val csvDir = Phase2Csv.institutionCsvDir(institutionPK)
      for (dt <- dataTypeList) {
        try {
          dt.writeToFile(csvDir, machineList)
        } catch {
          case t: Throwable => logger.error("Unexpected exception writing " + dt.getDataName + " : " + fmtEx(t))
        }
      }
      maintenanceCsv.writeToFile(csvDir, machineList)
    }

    // write the documentation for each type of data
    dataTypeList.foreach(dt => dt.writeDoc())

    maintenanceCsv.writeDoc()

    // Write an index for each institution, regardless of whether or not it has data.
    metadataCache.institutionNameMap.keys.foreach(institutionPK => Phase2Csv.generateIndex(institutionPK))

    Phase2Csv.writeCsvColumnDefinitions(dataTypeList)

    logger.info("Done with CSV generation.  Elapsed time: " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }

  /**
    * Make all the CSV files in a separate thread after the given delay.
    * @param delay_ms Wait this much time before starting.
    */
  def makeAllInFuture(delay_ms: Long): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    Future[Unit] {
      Thread.sleep(delay_ms)
      makeAll()
    }

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
    System.exit(0)
  }

}
