package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA
import org.aqa.Util

object AnalysisBBXZ {

  // Scala compiler has a problem flagging this import as 'unused'.
  private val WLBeam = org.aqa.webrun.wl.isoCheck.WLBeam

  private def BB_X(t: Int, row: Int): CsvCol[IC] = {
    val name = s"BB-X G180 C270 T$t"
    val description = s"BB-X G180 C270 T$t (mm) =Analysis!H$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
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
        ic.getBeam(180, 270, Util.negateAngle(t)) match {
          case Some(wl) if ic.hasTable =>
            ic.isoTable.get.BB_Z(WLBeam(wl))
          case _ => org.aqa.webrun.phase2.phase2csv.CsvCol.NA
        }
      }
    )
  }

  def tableBBXZ(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
      BB_X(  0, 14),
      BB_X( 30, 15),
      BB_X( 60, 16),
      BB_X( 90, 17),
      BB_X(270, 18),
      BB_X(300, 19),
      BB_X(330, 20),

      BB_Z(  0, 14),
      BB_Z( 30, 15),
      BB_Z( 60, 16),
      BB_Z( 90, 17),
      BB_Z(270, 18),
      BB_Z(300, 19),
      BB_Z(330, 20),
    )
    // @formatter:on
    list
  }

}
