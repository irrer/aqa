package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.DbSetup

/**
 * Generate all CSV files.
 */
object Phase2CsvMakeAll {

  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new PopulateDicomCsv).populateAll() // Get the DICOM column data up to date.

    (new CenterDoseCsv).updateFiles()
    (new CollimatorCenteringCsv).updateFiles()
    (new CollimatorPositionCsv).updateFiles()
    (new LeafPositionCsv).updateFiles()
    MaintenanceCsv.updateFiles() // special because it is not associate with Output or DICOM
    (new SymmetryAndFlatnessCsv).updateFiles()
    (new VMAT_T2_DR_GSCsv).updateFiles()
    (new VMAT_T2_DG_RSCsv).updateFiles()
    (new VMAT_T3MLCSpeedCsv).updateFiles()
    (new WedgePointCsv).updateFiles()
    Phase2Csv.generateIndex()
  }

}
