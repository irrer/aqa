package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

/**
  * Collimator X Y and Z from Analysis sheet.
  */
object AnalysisCAXYZ {

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

  def analysisCAXYZ(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
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
      CA_Z(270, 270, 0, 11)
    )
    // @formatter:on

    list
  }


}
