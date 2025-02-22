package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC

object AnalysisMLC {

  def mlc(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(

      CsvCol("MLC offset X", "MLC offset total X (mm) =Analysis!S2", (ic: IC) => ic.isoCheck.mlcOffsetX),
      CsvCol("MLC offset X 90", "MLC offset 90 X (mm) =Analysis!S3", (ic: IC) => ic.isoCheck.mlcOffsetX_090),
      CsvCol("MLC offset X 270", "MLC offset 270 X (mm) =Analysis!S4", (ic: IC) => ic.isoCheck.mlcOffsetX_270),

      CsvCol("MLC offset Y", "MLC offset total Y (mm) =Analysis!T2", (ic: IC) => ic.isoCheck.mlcOffsetY),
      CsvCol("MLC offset Y 90", "MLC offset 90 Y (mm) =Analysis!T3", (ic: IC) => ic.isoCheck.mlcOffsetY_090),
      CsvCol("MLC offset Y 270", "MLC offset 270 Y (mm) =Analysis!T4", (ic: IC) => ic.isoCheck.mlcOffsetY_270),

      CsvCol("CBCT Rel BB X", "CBCT origin relative to BB at table zero X (mm) =Analysis!V3", (_: IC) => 0.0),
      CsvCol("CBCT Rel BB Y", "CBCT origin relative to BB at table zero Y (mm) =Analysis!W3", (_: IC) => 0.0),
      CsvCol("CBCT Rel BB Z", "CBCT origin relative to BB at table zero Z (mm) =Analysis!X3", (_: IC) => 0.0),

      CsvCol("Coll-X G0", "Collimator X Gantry 0 =Analysis!I3", (ic: IC) => ic.isoCheck.collXG__0),
      CsvCol("Coll-X G180", "Collimator X Gantry 180 =Analysis!I8", (ic: IC) => ic.isoCheck.collXG180),

      CsvCol("Coll-Y G90", "Collimator Y Gantry 90 =Analysis!J5", (ic: IC) => ic.isoCheck.collYG_90),
      CsvCol("Coll-Y G270", "Collimator Y Gantry 270 =Analysis!J10", (ic: IC) => ic.isoCheck.collYG270),

      CsvCol("Coll-Z G0", "Collimator Z Gantry 0 =Analysis!K3", (ic: IC) => ic.isoCheck.collZG__0),
      CsvCol("Coll-Z G90", "Collimator Z Gantry 90 =Analysis!K5", (ic: IC) => ic.isoCheck.collZG_90),
      CsvCol("Coll-Z G180", "Collimator Z Gantry 180 =Analysis!K8", (ic: IC) => ic.isoCheck.collZG180),
      CsvCol("Coll-Z G270", "Collimator Z Gantry 270 =Analysis!K10", (ic: IC) => ic.isoCheck.collZG270),

    )
    // @formatter:on
    list
  }


}
