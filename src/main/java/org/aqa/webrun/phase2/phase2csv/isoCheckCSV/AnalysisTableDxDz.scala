package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA
import org.aqa.Util

object AnalysisTableDxDz {
  // Scala compiler has a problem flagging this import as 'unused'.
  private val WLBeam = org.aqa.webrun.wl.isoCheck.WLBeam

  private def dX(t: Int, row: Int): CsvCol[IC] = {
    val name = s"dX G180 C270 T$t"
    val description = s"dX G180 C270 T$t (mm) =Analysis!L$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
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
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.dZOf(WLBeam(wl), ic.isoTable.get.get_dXT__0_Optimized, ic.isoTable.get.get_dZT__0_Optimized)
          case _ => NA
        }
      }
    )
  }

  def tableDxDz(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
      dX( 30, 15),
      dX( 60, 16),
      dX( 90, 17),
      dX( 270, 18),
      dX( 300, 19),
      dX(330, 20),

      dZ( 30, 15),
      dZ( 60, 16),
      dZ( 90, 17),
      dZ(270, 18),
      dZ(300, 19),
      dZ(330, 20),
    )
    // @formatter:on

    list
  }


}
