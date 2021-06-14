package org.aqa.webrun.phase2.phase2csv

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DbSetup

/**
  * Generate all CSV files and documentation files.
  */
object Phase2CsvMakeAll extends Logging {

  def main(args: Array[String]): Unit = {
    DbSetup.init

    val start = System.currentTimeMillis()
    logger.info("Database has been initialized.  Starting CSV generation.")

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
        }
        catch {
          case t: Throwable => logger.error("Unexpected exception writing " + dt.getDataName + " : " + fmtEx(t))
        }
      }
      MaintenanceCsv.writeToFile(csvDir, machineList)
    }

    // write the documentation for each type of data
    dataTypeList.foreach(dt => dt.writeDoc())

    // Write an index for each institution, regardless of whether or not it has data.
    metadataCache.institutionNameMap.keys.foreach(institutionPK => Phase2Csv.generateIndex(institutionPK))

    logger.info("Done with CSV generation.  Elapsed time: " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }

}
