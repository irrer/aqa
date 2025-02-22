package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

object BBXZp {
  // Scala compiler has a problem flagging this import as 'unused'.
  private val WLBeam = org.aqa.webrun.wl.isoCheck.WLBeam

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

  def tableDxDz(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
      BB_Xp(  0, 14),
      BB_Xp( 30, 15),
      BB_Xp( 60, 16),
      BB_Xp( 90, 17),
      BB_Xp(270, 18),
      BB_Xp(300, 19),
      BB_Xp(330, 20),

      BB_Zp(  0, 14),
      BB_Zp( 30, 15),
      BB_Zp( 60, 16),
      BB_Zp( 90, 17),
      BB_Zp(270, 18),
      BB_Zp(300, 19),
      BB_Zp(330, 20),
    )
    // @formatter:on

    list
  }


}
