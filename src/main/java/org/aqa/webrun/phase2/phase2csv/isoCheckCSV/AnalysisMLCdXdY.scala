package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC

object AnalysisMLCdXdY {
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

  def mlcDxDy(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(

      MLC_dx("G0C90"   , 3, (ic: IC) => ic.isoCheck.mlcDxG__0_C_90 ),
      MLC_dx("G0C270"  , 4, (ic: IC) => ic.isoCheck.mlcDxG__0_C270 ),

      MLC_dx("G0C90"   , 5, (ic: IC) => ic.isoCheck.mlcDxG_90_C_90 ),
      MLC_dx("G0C270"  , 6, (ic: IC) => ic.isoCheck.mlcDxG_90_C270 ),

      MLC_dx("G180C0"  , 7, (ic: IC) => ic.isoCheck.mlcDxG180_C__0 ),
      MLC_dx("G180C90" , 8, (ic: IC) => ic.isoCheck.mlcDxG180_C_90 ),
      MLC_dx("G180C270", 9, (ic: IC) => ic.isoCheck.mlcDxG180_C270 ),

      MLC_dx("G270C90" , 10, (ic: IC) => ic.isoCheck.mlcDxG270_C_90),
      MLC_dx("G270C270", 11, (ic: IC) => ic.isoCheck.mlcDxG270_C270),

      // -----------------------------------------------------------------

      MLC_dy("G0C90"   ,  3, (ic: IC) => ic.isoCheck.mlcDyG__0_C_90),
      MLC_dy("G0C270"  ,  4, (ic: IC) => ic.isoCheck.mlcDyG__0_C270),

      MLC_dy("G0C90"   ,  5, (ic: IC) => ic.isoCheck.mlcDyG_90_C_90),
      MLC_dy("G0C270"  ,  6, (ic: IC) => ic.isoCheck.mlcDyG_90_C270),

      MLC_dy("G180C0"  ,  7, (ic: IC) => ic.isoCheck.mlcDyG180_C__0),
      MLC_dy("G180C90" ,  8, (ic: IC) => ic.isoCheck.mlcDyG180_C_90),
      MLC_dy("G180C270",  9, (ic: IC) => ic.isoCheck.mlcDyG180_C270),

      MLC_dy("G270C90" , 10, (ic: IC) => ic.isoCheck.mlcDyG270_C_90),
      MLC_dy("G270C270", 11, (ic: IC) => ic.isoCheck.mlcDyG270_C270),

    )
    // @formatter:on
    list
  }


}
