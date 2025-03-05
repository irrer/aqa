package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

object TableAxisXZ {

  def tableAxisXZ(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
      CsvCol("Table Rel BB X", "Table axis relative to BB at table zero X (mm) =Analysis!V5", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_X_Optimized else NA),
      CsvCol("Table Rel BB Z", "Table axis relative to BB at table zero Z (mm) =Analysis!X5", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_Z_Optimized else NA)
    )
    // @formatter:on

    list
  }


}
