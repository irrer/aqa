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

import org.aqa.db.IsoCheck
import org.aqa.db.Output
import org.aqa.webrun.wl.isoCheck.WLBeam

class IsoCheckCsv(metadataCache: MetadataCache) extends Phase2Csv[IsoCheck.IsoCheckHistory](metadataCache: MetadataCache) {

  // abbreviation for the long name
  private type IC = IsoCheck.IsoCheckHistory

  override val dataName: String = "IsoCheck"

  private val NA = "NA"

  private def MLC_dx(beamName: String, row: Int, func: IC => Any): CsvCol[IC] = {
    val name = s"$beamName MLC-dx"
    val description = s"$beamName MLC-dx =Analysis!Q$row"
    CsvCol(name, description, func)
  }

  private def MLC_dy(beamName: String, row: Int, func: IC => Any): CsvCol[IC] = {
    val name = s"$beamName MLC-dy"
    val description = s"$beamName MLC-dy =Analysis!R$row"
    CsvCol(name, description, func)
  }

  private def XOffset(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"X offset G$g C$c T$t"
    val description = s"X offset corrected box-ball Gantry:$g Collimator:$c Table$t (mm) =Analysis!D$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c, t) match {
          case Some(wl) => wl.errorX_mm
          case _        => NA
        }
      }
    )
  }

  private def YOffset(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"Y offset G$g C$c T$t"
    val description = s"Y offset corrected box-ball Gantry:$g Collimator:$c Table$t (mm) =Analysis!E$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c, t) match {
          case Some(wl) => wl.errorY_mm
          case _        => NA
        }
      }
    )
  }

  private def CA_X(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"CA-X G$g C$c T$t"
    val description = s"CA-X Gantry:$g Collimator:$c Table$t (mm) =Analysis!F$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c, t) match {
          case Some(wl) if wl.caX.isDefined => wl.caX.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_Y(g: Int, c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-Y G$g C$c T0"
    val description = s"CA-Y Gantry:$g Collimator:$c Table:0 (mm) =Analysis!G$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c) match {
          case Some(wl) if wl.caY.isDefined => wl.caY.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_Z(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"CA-Z G$g C$c T$t"
    val description = s"CA-Z Gantry:$g Collimator:$c Table$t (mm) =Analysis!H$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(g, c, t) match {
          case Some(wl) if wl.caZ.isDefined => wl.caZ.get
          case _                            => NA
        }
      }
    )
  }

  private def CA_ZT(t: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-Z G180 C270 T$t"
    val description = s"CA-Z Gantry:180 Collimator:270 Table$t (mm) =Analysis!G$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if wl.caZ.isDefined => wl.caZ.get
          case _                            => NA
        }
      }
    )
  }

  private def BB_Xpp(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-X'' G180 C270 T$t"
    val description = s"BB-X'' G180 C270 T$t (mm) =Analysis!P$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Xpp(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized, ic.isoTable.get.get_IsoTable_X_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def BB_Zpp(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-Z'' G180 C270 T$t"
    val description = s"BB-Z'' G180 C270 T$t (mm) =Analysis!Q$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Zpp(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized, ic.isoTable.get.get_IsoTable_Z_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def BB_RppSq(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-R''^2 G180 C270 T$t"
    val description = s"BB-R''^2 G180 C270 T$t (mm) =Analysis!R$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Rpp(
              WLBeam(wl),
              ic.isoTable.get.get_dXT__0_Optimized,
              ic.isoTable.get.get_dZT__0_Optimized,
              ic.isoTable.get.get_IsoTable_X_Optimized,
              ic.isoTable.get.get_IsoTable_Z_Optimized
            )
          case _ => NA
        }
      }
    )
  }

  private def BB_Xp(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-X' G180 C270 T$t"
    val description = s"BB-X' G180 C270 T$t (mm) =Analysis!J$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Xp(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def BB_Zp(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-Z' G180 C270 T$t"
    val description = s"BB-Z' G180 C270 T$t (mm) =Analysis!K$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Zp(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def BB_X(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-X G180 C270 T$t"
    val description = s"BB-X G180 C270 T$t (mm) =Analysis!H$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_X(WLBeam(wl))
          case _ => NA
        }
      }
    )
  }

  private def BB_Z(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-Z G180 C270 T$t"
    val description = s"BB-Z G180 C270 T$t (mm) =Analysis!I$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Z(WLBeam(wl))
          case _ => NA
        }
      }
    )
  }

  private def dX(t: Int, row: Int): CsvCol[IC] = {
    val name = s"dX G180 C270 T$t"
    val description = s"dX G180 C270 T$t (mm) =Analysis!L$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.dXOf(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def dZ(t: Int, row: Int): CsvCol[IC] = {
    val name = s"dZ G180 C270 T$t"
    val description = s"dZ G180 C270 T$t (mm) =Analysis!M$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, t) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.dZOf(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def CA_Xpp(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-X'' G180 C$c T0"
    val description = s"CA-X'' G180 C$c T0 (mm) =Collimator!K$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Xpp(WLBeam(wl), ic.collimator.getColl_X_Optimized)
          case _ => NA
        }
      }
    )
  }

  private def CA_Zpp(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-Z'' G180 C$c T0"
    val description = s"CA-X'' G180 C$c T0 (mm) =Collimator!K$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Zpp(WLBeam(wl), ic.collimator.getColl_Z_Optimized)
          case _ => NA
        }
      }
    )
  }


  private def CA_RppSq(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-R''^2 G180 C$c T0"
    val description = s"CA-R''^2 G180 C$c T0 (mm) =Collimator!L$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Rpp(WLBeam(wl), ic.collimator.getColl_X_Optimized, ic.collimator.getColl_Z_Optimized)
          case _ => NA
        }
      }
    )
  }


  override protected def makeColList: Seq[CsvCol[IC]] = {
    Seq(
      // @formatter:off
      CsvCol("CBCT - Gantry Iso X", "CBCT - Gantry Iso X (mm) in room coordinates =SNCImport!C2 =Report!C4", (ic: IC) => -ic.isoCheck.isoX),
      CsvCol("CBCT - Gantry Iso Y", "CBCT - Gantry Iso Y (mm) in room coordinates =SNCImport!C3 =Report!D4", (ic: IC) => -ic.isoCheck.isoY),
      CsvCol("CBCT - Gantry Iso Z", "CBCT - Gantry Iso Z (mm) in room coordinates =SNCImport!C4 =Report!E4", (ic: IC) => -ic.isoCheck.isoZ),
      CsvCol("Table - Gantry Iso X", "Table - Gantry Iso X (mm) in room coordinates =SNCImport!C5 =Report!F4", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_X_Optimized - ic.isoCheck.isoX else NA),
      CsvCol("Table - Gantry Iso Z", "Table - Gantry Iso Z (mm) in room coordinates =SNCImport!C6 =Report!G4", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_Z_Optimized - ic.isoCheck.isoZ else NA),
      CsvCol("Gantry Flex", "Gantry Flex (mm)  in room coordinates =SNCImport!C7 =Report!H4", (ic: IC) => ic.isoCheck.gantryFlex),
      CsvCol("Col-Gantry misalignment", "Col-Gantry misalignment (mm) in room coordinates =SNCImport!C8 =Report!I4", (ic: IC) => ic.isoCheck.collGantryMisalign),
      CsvCol("MLC offset", "MLC offset (mm) in room coordinates =SNCImport!C9 =Report!J4", (ic: IC) => ic.isoCheck.mlcOffsetY),
      CsvCol("Table Isocentricity", "Table Isocentricity (mm) in room coordinates =SNCImport!C10.", (ic: IC) => if (ic.hasTable) Math.sqrt(ic.isoTable.get.get_RSquared_Optimized) else NA),
      CsvCol("Gantry Isocentricity", "Gantry Isocentricity (mm) in room coordinates =SNCImport!C11", (ic: IC) => ic.isoCheck.gantryIsocentricity),
      CsvCol("Collimator Isocentricity", "Collimator Isocentricity (mm) in room coordinates =SNCImport!C12 =Collimator!L2", (ic: IC) => ic.collimator.getCA_Rpp_Optimized),

      CsvCol("Table Wobble", "Table Wobble diameter (mm) =Report!K4", (ic: IC) => ic.isoTable.get.tableWobbleDiameter),

      CsvCol("ballCBCT X", "ballCBCT X (mm) =Analysis!A24", (_: IC) => 0.0),
      CsvCol("ballCBCT Y", "ballCBCT Y (mm) =Analysis!B24", (_: IC) => 0.0),
      CsvCol("ballCBCT Z", "ballCBCT Z (mm) =Analysis!C24", (_: IC) => 0.0),

      CsvCol("CBCT-ball-table0 X", "CBCT-ball-table0 X (mm) =Analysis!A27", (_: IC) => 0.0),
      CsvCol("CBCT-ball-table0 Y", "CBCT-ball-table0 Y (mm) =Analysis!B27", (_: IC) => 0.0),
      CsvCol("CBCT-ball-table0 Z", "CBCT-ball-table0 Z (mm) =Analysis!C27", (_: IC) => 0.0),

      CsvCol("Coll-Gantry-misalign", "Coll-Gantry-misalign (mm) =Analysis!P3", (ic: IC) => ic.isoCheck.collGantryMisalign),
      CsvCol("Coll-Gantry-misalign", "Coll-Gantry-misalign (mm) =Analysis!P12", (ic: IC) => ic.isoCheck.collGantryMisalign),

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

      BB_RppSq(0, 14),
      BB_RppSq(30, 15),
      BB_RppSq(60, 16),
      BB_RppSq(90, 17),
      BB_RppSq(270, 18),
      BB_RppSq(300, 19),
      BB_RppSq(330, 20),

      dX(30, 15),
      dX(60, 16),
      dX(90, 17),
      dX(270, 18),
      dX(300, 19),
      dX(330, 20),

      dZ(30, 15),
      dZ(60, 16),
      dZ(90, 17),
      dZ(270, 18),
      dZ(300, 19),
      dZ(330, 20),

      BB_Xpp(0, 14),
      BB_Xpp(30, 15),
      BB_Xpp(60, 16),
      BB_Xpp(90, 17),
      BB_Xpp(270, 18),
      BB_Xpp(300, 19),
      BB_Xpp(330, 20),

      BB_Zpp(0, 14),
      BB_Zpp(30, 15),
      BB_Zpp(60, 16),
      BB_Zpp(90, 17),
      BB_Zpp(270, 18),
      BB_Zpp(300, 19),
      BB_Zpp(330, 20),

      BB_Xp(0, 14),
      BB_Xp(30, 15),
      BB_Xp(60, 16),
      BB_Xp(90, 17),
      BB_Xp(270, 18),
      BB_Xp(300, 19),
      BB_Xp(330, 20),

      BB_Zp(0, 14),
      BB_Zp(30, 15),
      BB_Zp(60, 16),
      BB_Zp(90, 17),
      BB_Zp(270, 18),
      BB_Zp(300, 19),
      BB_Zp(330, 20),

      BB_X(0, 14),
      BB_X(30, 15),
      BB_X(60, 16),
      BB_X(90, 17),
      BB_X(270, 18),
      BB_X(300, 19),
      BB_X(330, 20),

      BB_Z(0, 14),
      BB_Z(30, 15),
      BB_Z(60, 16),
      BB_Z(90, 17),
      BB_Z(270, 18),
      BB_Z(300, 19),
      BB_Z(330, 20),

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

      // -----------------------------------------------------------------

      MLC_dx("G0C90", 3, (ic: IC) => ic.isoCheck.mlcDxG__0_C_90),
      MLC_dx("G0C270", 4, (ic: IC) => ic.isoCheck.mlcDxG__0_C270),

      MLC_dx("G0C90", 5, (ic: IC) => ic.isoCheck.mlcDxG_90_C_90),
      MLC_dx("G0C270", 6, (ic: IC) => ic.isoCheck.mlcDxG_90_C270),

      MLC_dx("G180C0", 7, (ic: IC) => ic.isoCheck.mlcDxG180_C__0),
      MLC_dx("G180C90", 8, (ic: IC) => ic.isoCheck.mlcDxG180_C_90),
      MLC_dx("G180C270", 9, (ic: IC) => ic.isoCheck.mlcDxG180_C270),

      MLC_dx("G270C90", 10, (ic: IC) => ic.isoCheck.mlcDxG270_C_90),
      MLC_dx("G270C270", 11, (ic: IC) => ic.isoCheck.mlcDxG270_C270),

      // -----------------------------------------------------------------

      MLC_dy("G0C90", 3, (ic: IC) => ic.isoCheck.mlcDyG__0_C_90),
      MLC_dy("G0C270", 4, (ic: IC) => ic.isoCheck.mlcDyG__0_C270),

      MLC_dy("G0C90", 5, (ic: IC) => ic.isoCheck.mlcDyG_90_C_90),
      MLC_dy("G0C270", 6, (ic: IC) => ic.isoCheck.mlcDyG_90_C270),

      MLC_dy("G180C0", 7, (ic: IC) => ic.isoCheck.mlcDyG180_C__0),
      MLC_dy("G180C90", 8, (ic: IC) => ic.isoCheck.mlcDyG180_C_90),
      MLC_dy("G180C270", 9, (ic: IC) => ic.isoCheck.mlcDyG180_C270),

      MLC_dy("G270C90", 10, (ic: IC) => ic.isoCheck.mlcDyG270_C_90),
      MLC_dy("G270C270", 11, (ic: IC) => ic.isoCheck.mlcDyG270_C270),

      // -----------------------------------------------------------------

      CsvCol("Coll-X", "Collimator X =Collimator!H4", (ic: IC) => ic.collimator.getColl_X_Optimized),
      CsvCol("Coll-Z", "Collimator Z =Collimator!I4", (ic: IC) => ic.collimator.getColl_Z_Optimized),

      CA_RppSq(  0, 4),
      CA_RppSq( 90, 5),
      CA_RppSq(270, 6),

      CA_Xpp(  0, 4),
      CA_Xpp( 90, 5),
      CA_Xpp(270, 6),

      CA_Zpp(  0, 4),
      CA_Zpp( 90, 5),
      CA_Zpp(270, 6),

      // -----------------------------------------------------------------

      XOffset(  0,  90, 0,  3),
      XOffset(  0, 270, 0,  4),
      XOffset( 90,  90, 0,  5),
      XOffset( 90, 270, 0,  6),
      XOffset(180,   0, 0,  7, " =Collimator!D4"),
      XOffset(180,  90, 0,  8, " =Collimator!D5"),
      XOffset(180, 270, 0,  9, " =Collimator!D6"),
      XOffset(270,  90, 0, 10),
      XOffset(270, 270, 0, 11),

      XOffset(180, 270,  30, 15),
      XOffset(180, 270,  60, 16),
      XOffset(180, 270,  90, 17),
      XOffset(180, 270, 270, 18),
      XOffset(180, 270, 300, 19),
      XOffset(180, 270, 330, 20),

      // -----------------------------------------------------------------

      YOffset(  0,  90, 0,  3),
      YOffset(  0, 270, 0,  4),
      YOffset( 90,  90, 0,  5),
      YOffset( 90, 270, 0,  6),
      YOffset(180,   0, 0,  7, " =Collimator!D4"),
      YOffset(180,  90, 0,  8, " =Collimator!D5"),
      YOffset(180, 270, 0,  9, " =Collimator!D6"),
      YOffset(270,  90, 0, 10),
      YOffset(270, 270, 0, 11),

      YOffset(180, 270,  30, 15),
      YOffset(180, 270,  60, 16),
      YOffset(180, 270,  90, 17),
      YOffset(180, 270, 270, 18),
      YOffset(180, 270, 300, 19),
      YOffset(180, 270, 330, 20),

      // -----------------------------------------------------------------

      CA_X(  0,  90, 0, 3),
      CA_X(  0, 270, 0, 4),

      CA_X(180,   0, 0, 7, " =Collimator!F4"),
      CA_X(180,  90, 0, 8, " =Collimator!F5"),
      CA_X(180, 270, 0, 9, " =Collimator!F6"),

      CA_X(180, 270, 30, 15),
      CA_X(180, 270, 60, 16),
      CA_X(180, 270, 90, 17),
      CA_X(180, 270, 270, 18),
      CA_X(180, 270, 300, 19),
      CA_X(180, 270, 330, 20),

      // -----------------------------------------------------------------

      CA_Y( 90,  90,  5),
      CA_Y( 90, 270,  6),
      CA_Y(270,  90, 10),
      CA_Y(270, 270, 11),

      // -----------------------------------------------------------------

      CA_Z(  0,  90, 0,  3),
      CA_Z(  0, 270, 0,  4),
      CA_Z( 90,  90, 0,  5),
      CA_Z( 90, 270, 0,  6),

      CA_Z(180,   0, 0,  7, " =Collimator!G4"),
      CA_Z(180,  90, 0,  8, " =Collimator!G5"),
      CA_Z(180, 270, 0,  9, " =Collimator!G6"),

      CA_Z(270, 90,  0, 10),
      CA_Z(270, 270, 0, 11),

      CA_ZT( 30, 15),
      CA_ZT( 60, 16),
      CA_ZT( 90, 17),
      CA_ZT(270, 18),
      CA_ZT(300, 19),
      CA_ZT(330, 20),

      // -----------------------------------------------------------------

      CsvCol("Table Rel BB X", "Table axis relative to BB at table zero X (mm) =Analysis!T4", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_X_Optimized else NA),
      CsvCol("Table Rel BB Z", "Table axis relative to BB at table zero Z (mm) =Analysis!X4", (ic: IC) => if (ic.hasTable) ic.isoTable.get.get_IsoTable_Z_Optimized else NA)

      // @formatter:on
    )
  }

  /**
   * Get the data for a particular machine.
   *
   * @param machinePK Machine to get data for.
   * @return List of data for the particular machine.
   */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[IC] = {
    val cdHistory = IsoCheck.history(machinePK)
    cdHistory
  }

  override def getSopUidList(data: IC): Seq[String] = {
    val firstBeam = data.getBeam(0, 90).get.rtimageUID
    Seq(firstBeam)
  }

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

  override def getOutput(data: IC): Output = data.output
}
