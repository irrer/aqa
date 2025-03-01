package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

/**
  * Summary information from analysis sheet.
  */
object AnalysisSummary {
  def analysisSummaryList(): Seq[CsvCol[IC]] = {
    // @formatter:off
    Seq(
      CsvCol("Number of Beams", "Total number of beams/RTIMAGES used for IsoCheck analysis.  Must be one of 9, 11, or 15", (ic: IC) => if (ic.hasTable) 8 + ic.isoTable.get.beamList.size else 9),

      CsvCol("Table Wobble", "Table Wobble diameter (mm) =Report!K4", (ic: IC) => if (ic.hasTable) ic.isoTable.get.tableWobbleDiameter else NA),

      CsvCol("ballCBCT X", "ballCBCT X (mm) =Analysis!A24", (_: IC) => 0.0),
      CsvCol("ballCBCT Y", "ballCBCT Y (mm) =Analysis!B24", (_: IC) => 0.0),
      CsvCol("ballCBCT Z", "ballCBCT Z (mm) =Analysis!C24", (_: IC) => 0.0),

      CsvCol("CBCT-ball-table0 X", "CBCT-ball-table0 X (mm) =Analysis!A27", (_: IC) => 0.0),
      CsvCol("CBCT-ball-table0 Y", "CBCT-ball-table0 Y (mm) =Analysis!B27", (_: IC) => 0.0),
      CsvCol("CBCT-ball-table0 Z", "CBCT-ball-table0 Z (mm) =Analysis!C27", (_: IC) => 0.0),

      CsvCol("ISO-X", "ISO-X (mm) =Analysis!L3", (ic: IC) => ic.isoCheck.isoX),
      CsvCol("ISO-Y", "ISO-Y (mm) =Analysis!M3", (ic: IC) => ic.isoCheck.isoY),
      CsvCol("ISO-Z", "ISO-Z (mm) =Analysis!N3", (ic: IC) => ic.isoCheck.isoZ),

      CsvCol("Delta X", "ISO-X max-min (mm) =Analysis!M7", (ic: IC) => ic.isoCheck.isoXRange),
      CsvCol("Delta Y", "ISO-Y max-min (mm) =Analysis!N7", (ic: IC) => ic.isoCheck.isoYRange),
      CsvCol("Delta Z", "ISO-Z max-min (mm) =Analysis!O7", (ic: IC) => ic.isoCheck.isoZRange),

      CsvCol("Gantry Isocentricity", "Gantry Isocentricity (mm) =Analysis!O8", (ic: IC) => ic.isoCheck.gantryIsocentricity),
      CsvCol("Couch Isocentricity", "Couch Isocentricity (mm) =Analysis!R12", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_RSquared_Optimized else NA),

      CsvCol("dX", "dX =Analysis!L14", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_dXT__0_Optimized else NA),
      CsvCol("dZ", "dZ =Analysis!M14", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_dZT__0_Optimized else NA),
      CsvCol("Table-X", "Table X =Analysis!N14", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_X_Optimized else NA),
      CsvCol("Table-Z", "Table Z =Analysis!O14", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_Z_Optimized else NA),
    )
    // @formatter:on
  }

}
