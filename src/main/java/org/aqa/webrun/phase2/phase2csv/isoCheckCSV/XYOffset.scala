package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

object XYOffset {

  private def XOffset(g: Int, c: Int, t: Int, row: Int, extraRef: String = ""): CsvCol[IC] = {
    val name = s"X offset G$g C$c T$t"
    val description = s"X offset corrected box-ball Gantry:$g Collimator:$c Table$t (mm) =Analysis!D$row" + extraRef

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.wlMap.find(g, c, t) match {
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
        ic.wlMap.find(g, c, t) match {
          case Some(wl) => wl.errorY_mm
          case _        => NA
        }
      }
    )
  }

  def XYOffsetList(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(
      // gantry beams
      XOffset(  0,  90, 0,  3),
      XOffset(  0, 270, 0,  4),
      XOffset( 90,  90, 0,  5),
      XOffset( 90, 270, 0,  6),
      XOffset(180,   0, 0,  7, " =Collimator!D4"),
      XOffset(180,  90, 0,  8, " =Collimator!D5"),
      XOffset(180, 270, 0,  9, " =Collimator!D6"),
      XOffset(270,  90, 0, 10),
      XOffset(270, 270, 0, 11),

      // table beams
      XOffset(180, 270,  30, 15),
      XOffset(180, 270,  60, 16),
      XOffset(180, 270,  90, 17),
      XOffset(180, 270, 270, 18),
      XOffset(180, 270, 300, 19),
      XOffset(180, 270, 330, 20),

      // gantry beams
      YOffset(  0,  90, 0,  3),
      YOffset(  0, 270, 0,  4),
      YOffset( 90,  90, 0,  5),
      YOffset( 90, 270, 0,  6),
      YOffset(180,   0, 0,  7, " =Collimator!D4"),
      YOffset(180,  90, 0,  8, " =Collimator!D5"),
      YOffset(180, 270, 0,  9, " =Collimator!D6"),
      YOffset(270,  90, 0, 10),
      YOffset(270, 270, 0, 11),

      // table beams
      YOffset(180, 270,  30, 15),
      YOffset(180, 270,  60, 16),
      YOffset(180, 270,  90, 17),
      YOffset(180, 270, 270, 18),
      YOffset(180, 270, 300, 19),
      YOffset(180, 270, 330, 20),
    )
    // @formatter:on

    list
  }

}
