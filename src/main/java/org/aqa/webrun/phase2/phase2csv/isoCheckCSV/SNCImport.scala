package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

object SNCImport {

  def sncImportList(): Seq[CsvCol[IC]] = {
    // @formatter:off
    Seq(
      CsvCol("CBCT - Gantry Iso X"     , "CBCT - Gantry Iso X (mm) in room coordinates =SNCImport!C2 =Report!C4"                   , (ic: IC) => -ic.isoCheck.isoX),
      CsvCol("CBCT - Gantry Iso Y"     , "CBCT - Gantry Iso Y (mm) in room coordinates =SNCImport!C3 =Report!D4"                   , (ic: IC) => -ic.isoCheck.isoY),
      CsvCol("CBCT - Gantry Iso Z"     , "CBCT - Gantry Iso Z (mm) in room coordinates =SNCImport!C4 =Report!E4"                   , (ic: IC) => -ic.isoCheck.isoZ),
      CsvCol("Table - Gantry Iso X"    , "Table - Gantry Iso X (mm) in room coordinates =SNCImport!C5 =Report!F4"                  , (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_X_Optimized - ic.isoCheck.isoX else NA),
      CsvCol("Table - Gantry Iso Z"    , "Table - Gantry Iso Z (mm) in room coordinates =SNCImport!C6 =Report!G4"                  , (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_Z_Optimized - ic.isoCheck.isoZ else NA),
      CsvCol("Gantry Flex"             , "Gantry Flex (mm)  in room coordinates =SNCImport!C7 =Report!H4"                          , (ic: IC) => ic.isoCheck.gantryFlex),
      CsvCol("Col-Gantry misalignment" , "Col-Gantry misalignment (mm) in room coordinates =SNCImport!C8 =Report!I4  =Analysis!P3" , (ic: IC) => ic.isoCheck.collGantryMisalign),
      CsvCol("MLC offset"              , "MLC offset (mm) in room coordinates =SNCImport!C9 =Report!J4"                            , (ic: IC) => ic.isoCheck.mlcOffsetY),
      CsvCol("Table Isocentricity"     , "Table Isocentricity (mm) in room coordinates =SNCImport!C10."                            , (ic: IC) => if (ic.hasTable) Math.sqrt(ic.isoTable.get.get_RSquared_Optimized) else NA),
      CsvCol("Gantry Isocentricity"    , "Gantry Isocentricity (mm) in room coordinates =SNCImport!C11"                            , (ic: IC) => ic.isoCheck.gantryIsocentricity),
      CsvCol("Collimator Isocentricity", "Collimator Isocentricity (mm) in room coordinates =SNCImport!C12 =Collimator!L2"         , (ic: IC) => ic.collimator.get_CA_Rpp_Optimized),
    )
    // @formatter:on
  }
}
