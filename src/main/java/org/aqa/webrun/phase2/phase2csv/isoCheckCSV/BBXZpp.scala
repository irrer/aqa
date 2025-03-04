package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA
import org.aqa.Util

/**
  * X and Y offset corrected box - ball
  */
object BBXZpp {

  private def BB_RppSq(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-R''^2 G180 C270 T$t"
    val description = s"BB-R''^2 G180 C270 T$t (mm) =Analysis!R$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Rpp(
              wl,
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

  private def BB_Xpp(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-X'' G180 C270 T$t"
    val description = s"BB-X'' G180 C270 T$t (mm) =Analysis!P$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Xpp(wl, ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized, ic.isoTable.get.get_IsoTable_X_Optimized)
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
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Zpp(wl, ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized, ic.isoTable.get.get_IsoTable_Z_Optimized)
          case _ => NA
        }
      }
    )
  }

  def analysisBBpp(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
      BB_RppSq(  0, 14),
      BB_RppSq( 30, 15),
      BB_RppSq( 60, 16),
      BB_RppSq( 90, 17),
      BB_RppSq(270, 18),
      BB_RppSq(300, 19),
      BB_RppSq(330, 20),

      BB_Xpp(  0, 14),
      BB_Xpp( 30, 15),
      BB_Xpp( 60, 16),
      BB_Xpp( 90, 17),
      BB_Xpp(270, 18),
      BB_Xpp(300, 19),
      BB_Xpp(330, 20),

      BB_Zpp(  0, 14),
      BB_Zpp( 30, 15),
      BB_Zpp( 60, 16),
      BB_Zpp( 90, 17),
      BB_Zpp(270, 18),
      BB_Zpp(300, 19),
      BB_Zpp(330, 20),
    )
    // @formatter:on

    list
  }


}
