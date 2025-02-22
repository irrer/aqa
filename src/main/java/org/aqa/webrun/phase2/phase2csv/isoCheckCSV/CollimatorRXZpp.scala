package org.aqa.webrun.phase2.phase2csv.isoCheckCSV

import org.aqa.webrun.phase2.phase2csv.CsvCol
import org.aqa.webrun.phase2.phase2csv.isoCheckCSV.IsoCheckCsv.IC
import org.aqa.webrun.phase2.phase2csv.CsvCol.NA

object CollimatorRXZpp {

  // Scala compiler has a problem flagging this import as 'unused'.
  private val WLBeam = org.aqa.webrun.wl.isoCheck.WLBeam

  private def CA_RppSq(c: Int, row: Int): CsvCol[IC] = {
    val name = s"CA-R''^2 G180 C$c T0"
    val description = s"CA-R''^2 G180 C$c T0 (mm) =Collimator!L$row"

    CsvCol(
      name,
      description,
      (ic: IC) => {
        ic.getBeam(180, c) match {
          case Some(wl) =>
            ic.collimator.CA_Rpp(WLBeam(wl), ic.collimator.get_Coll_X_Optimized, ic.collimator.get_Coll_Z_Optimized)
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
            ic.collimator.CA_Xpp(WLBeam(wl), ic.collimator.get_Coll_X_Optimized)
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
            ic.collimator.CA_Zpp(WLBeam(wl), ic.collimator.get_Coll_Z_Optimized)
          case _ => NA
        }
      }
    )
  }

  def optimizedAndBBXZ(): Seq[CsvCol[IC]] = {
    // @formatter:off
    val list = Seq(

      CsvCol("Coll-X", "Collimator X =Collimator!H4", (ic: IC) => ic.collimator.get_Coll_X_Optimized),
      CsvCol("Coll-Z", "Collimator Z =Collimator!I4", (ic: IC) => ic.collimator.get_Coll_Z_Optimized),

      CA_RppSq(  0, 4),
      CA_RppSq( 90, 5),
      CA_RppSq(270, 6),

      CA_Xpp  (  0, 4),
      CA_Xpp  ( 90, 5),
      CA_Xpp  (270, 6),

      CA_Zpp  (  0, 4),
      CA_Zpp  ( 90, 5),
      CA_Zpp  (270, 6),
    )
    // @formatter:on
    list
  }

}
