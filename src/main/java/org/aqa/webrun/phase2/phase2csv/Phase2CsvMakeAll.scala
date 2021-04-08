package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.DbSetup

/**
 * Generate all CSV files.
 */
object Phase2CsvMakeAll {

  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new CenterDoseCsv).updateFiles()
    (new SymmetryAndFlatnessCsv).updateFiles()
    (new VMAT_T2_DR_GSCsv).updateFiles()
    (new VMAT_T2_DG_RSCsv).updateFiles()
    (new VMAT_T3MLCSpeedCsv).updateFiles()
    (new WedgePointCsv).updateFiles()
  }

}
