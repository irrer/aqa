package org.aqa.webrun.wl.wlMonthly

import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.rnd

/**
  * Contain and process WL monthly data.
  *
  * The naming convention is intentionally pedantic as an attempt to clarify the beams' specifications.
  *
  * The G, C, T abbreviations are for Gantry, Collimator, and Table angles.
  *
  * @param extendedData   metadata
  * @param G__0_C_90_T__0 beam
  * @param G__0_C270_T__0 beam
  * @param G_90_C_90_T__0 beam
  * @param G_90_C270_T__0 beam
  * @param G180_C__0_T__0 beam
  * @param G180_C_90_T__0 beam
  * @param G180_C270_T__0 beam
  * @param G270_C_90_T__0 beam
  * @param G270_C270_T__0 beam
  * @param G180_C270_T_90 beam
  * @param G180_C270_T270 beam
  */
case class WLMonthly( // @formatter:off
                      extendedData: ExtendedData,
                      G__0_C_90_T__0: WLBeam,
                      G__0_C270_T__0: WLBeam,

                      G_90_C_90_T__0: WLBeam,
                      G_90_C270_T__0: WLBeam,

                      G180_C__0_T__0: WLBeam,
                      G180_C_90_T__0: WLBeam,
                      G180_C270_T__0: WLBeam,

                      G270_C_90_T__0: WLBeam,
                      G270_C270_T__0: WLBeam,

                      G180_C270_T_90: WLBeam,
                      G180_C270_T270: WLBeam
                      // @formatter:on
                    ) extends Logging {

  val collXG__0: Double = rnd((G__0_C_90_T__0.caX.get + G__0_C270_T__0.caX.get) / 2) // Analysis I  3
  val collXG180: Double = rnd((G180_C_90_T__0.caX.get + G180_C270_T__0.caX.get) / 2) // Analysis I  8
  private val collXSeq: Seq[Double] = Seq(collXG__0, collXG180)

  val collYG_90: Double = rnd((G_90_C_90_T__0.caY.get + G_90_C270_T__0.caY.get) / 2) // Analysis J  5
  val collYG270: Double = rnd((G270_C_90_T__0.caY.get + G270_C270_T__0.caY.get) / 2) // Analysis J 10
  private val collYSeq: Seq[Double] = Seq(collYG_90, collYG270)

  val collZG__0: Double = rnd((G__0_C_90_T__0.caZ.get + G__0_C270_T__0.caZ.get) / 2) // Analysis K  3
  val collZG_90: Double = rnd((G_90_C_90_T__0.caZ.get + G_90_C270_T__0.caZ.get) / 2) // Analysis K  5
  val collZG180: Double = rnd((G180_C_90_T__0.caZ.get + G180_C270_T__0.caZ.get) / 2) // Analysis K  8
  val collZG270: Double = rnd((G270_C_90_T__0.caZ.get + G270_C270_T__0.caZ.get) / 2) // Analysis K 10
  private val collZSeq = Seq(collZG__0, collZG_90, collZG180, collZG270)

  val isoX: Double = rnd(collXSeq.sum / collXSeq.size) // Analysis L  3
  val isoY: Double = rnd(collYSeq.sum / collYSeq.size) // Analysis M  3
  val isoZ: Double = rnd(collZSeq.sum / collZSeq.size) // Analysis N  3

  if (true) { // TODO rm
    Trace.trace("G__0_C_90_T__0.wl: " + G__0_C_90_T__0.wl)
    Trace.trace("G__0_C270_T__0.wl: " + G__0_C270_T__0.wl)
    Trace.trace("G180_C_90_T__0.wl: " + G180_C_90_T__0.wl)
    Trace.trace("G180_C270_T__0.wl: " + G180_C270_T__0.wl)

    Trace.trace("G_90_C_90_T__0.wl: " + G__0_C_90_T__0.wl)
    Trace.trace("G_90_C270_T__0.wl: " + G__0_C270_T__0.wl)

    Trace.trace("collXG__0: " + collXG__0)
    Trace.trace("collXG180: " + collXG180)
    Trace.trace("collXSeq: " + collXSeq)

    Trace.trace("collYG_90: " + collYG_90)
    Trace.trace("collYG270: " + collYG270)
    Trace.trace("collYSeq: " + collYSeq)

    Trace.trace("collZG__0: " + collZG__0)
    Trace.trace("collZG_90: " + collZG_90)
    Trace.trace("collZG180: " + collZG180)
    Trace.trace("collZG270: " + collZG270)
    Trace.trace("collZSeq: " + collZSeq)

    Trace.trace("isoX: " + isoX)
    Trace.trace("isoY: " + isoY)
    Trace.trace("isoZ: " + isoZ)
    Trace.trace()
  }


  val isoXRange: Double = rnd(collXSeq.max - collXSeq.min) // Analysis M  7
  val isoYRange: Double = rnd(collYSeq.max - collYSeq.min) // Analysis N  7
  val isoZRange: Double = rnd(collZSeq.max - collZSeq.min) // Analysis O  7

  val gantryFlex: Double = rnd(collZG__0 - collZG180) // Analysis O  3

  val gantryIsocentricity: Double = {
    val coordinates = Seq(isoXRange, isoYRange, isoZRange)
    Trace.trace(s"coordinates: $coordinates")
    val sumSquares = coordinates.map(v => v * v).sum
    Trace.trace(s"sumSquares: $sumSquares")
    val distance = Math.sqrt(sumSquares)
    Trace.trace(s"distance: $distance")
    val O8 = rnd(distance / 2)
    Trace.trace(s"O8: $O8")
    O8
  } // Analysis O  8

  /* spreadsheet coordinates:                  I3-L3             J5-M3        L3-I8             M3-J10 */
  val collGantryMisalign: Double = rnd(Seq(collXG__0 - isoX, collYG_90 - isoY, isoX - collXG180, isoY - collYG270).sum / 4) // Analysis P  3

  // ------------------------------------------------------------------------------------------
  // mlcDx

  val mlcDxG__0_C_90: Double = rnd(G__0_C_90_T__0.caX.get - collXG__0) //  F3 -  I3 => Analysis Q  3
  val mlcDxG__0_C270: Double = rnd(G__0_C270_T__0.caX.get - collXG__0) //  F4 -  I3 => Analysis Q  4

  val mlcDxG_90_C_90: Double = rnd(G_90_C_90_T__0.caY.get - collYG_90) //  G4 -  J5 => Analysis Q  5
  val mlcDxG_90_C270: Double = rnd(G_90_C270_T__0.caY.get - collYG_90) //  G6 -  J5 => Analysis Q  6

  val mlcDxG180_C__0: Double = rnd(G180_C__0_T__0.caX.get - collXG180) //  F7 -  I8 => Analysis Q  7
  val mlcDxG180_C_90: Double = rnd(G180_C_90_T__0.caX.get - collXG180) //  F8 -  I8 => Analysis Q  8
  val mlcDxG180_C270: Double = rnd(G180_C270_T__0.caX.get - collXG180) //  F9 -  I8 => Analysis Q  9

  val mlcDxG270_C_90: Double = rnd(G270_C_90_T__0.caY.get - collYG270) // G10 - J10 => Analysis Q 10
  val mlcDxG270_C270: Double = rnd(G270_C270_T__0.caY.get - collYG270) // G11 - J10 => Analysis Q 11

  // ------------------------------------------------------------------------------------------
  // mlcDy

  val mlcDyG__0_C_90: Double = rnd(G__0_C_90_T__0.caZ.get - collZG__0) //  F3 -  I3 => Analysis R  3
  val mlcDyG__0_C270: Double = rnd(G__0_C270_T__0.caZ.get - collZG__0) //  F4 -  I3 => Analysis R  4

  val mlcDyG_90_C_90: Double = rnd(G_90_C_90_T__0.caZ.get - collZG_90) //  G4 -  J5 => Analysis R  5
  val mlcDyG_90_C270: Double = rnd(G_90_C270_T__0.caZ.get - collZG_90) //  G6 -  J5 => Analysis R  6

  val mlcDyG180_C__0: Double = rnd(G180_C__0_T__0.caZ.get - collZG180) //  F7 -  I8 => Analysis R  7
  val mlcDyG180_C_90: Double = rnd(G180_C_90_T__0.caZ.get - collZG180) //  F8 -  I8 => Analysis R  8
  val mlcDyG180_C270: Double = rnd(G180_C270_T__0.caZ.get - collZG180) //  F9 -  I8 => Analysis R  9

  val mlcDyG270_C_90: Double = rnd(G270_C_90_T__0.caZ.get - collZG270) // G10 - J10 => Analysis R 10
  val mlcDyG270_C270: Double = rnd(G270_C270_T__0.caZ.get - collZG270) // G11 - J10 => Analysis R 11

  // ------------------------------------------------------------------------------------------

  val mlcOffsetX_090: Double = rnd((mlcDxG__0_C_90 + mlcDxG_90_C_90 + mlcDxG180_C_90 + mlcDxG270_C_90) / 4) // Analysis S  3
  val mlcOffsetX_270: Double = rnd((mlcDxG__0_C270 + mlcDxG_90_C270 + mlcDxG180_C270 + mlcDxG270_C270) / 4) // Analysis S  4
  val mlcOffsetX: Double = rnd(mlcOffsetX_090 - mlcOffsetX_270) // Analysis S  2

  val mlcOffsetY_090: Double = rnd((mlcDyG__0_C_90 + mlcDyG_90_C_90 + mlcDyG180_C_90 + mlcDyG270_C_90) / 4) // Analysis T  3
  val mlcOffsetY_270: Double = rnd((mlcDyG__0_C270 + mlcDyG_90_C270 + mlcDyG180_C270 + mlcDyG270_C270) / 4) // Analysis T  4
  val mlcOffsetY: Double = rnd(mlcOffsetY_090 - mlcOffsetY_270) // Analysis T  2

  // ------------------------------------------------------------------------------------------


}

object WLMonthly extends Logging {

  /**
   * Determine if all the data is present to construct a WL Monthly data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param beamList     List of incoming DICOM and results.
   * @return Monthly data set or None.
   */
  def make(extendedData: ExtendedData, beamList: Seq[WLBeam]): Option[WLMonthly] = {

    def findPair(g: Int, c: Int, t: Int): Option[WLBeam] = {
      WLBeam.findGCT(beamList, g, c, t)
    }

    // @formatter:off
    val G__0_C_90_T__0: Option[WLBeam] = findPair(  0,  90,   0)
    val G__0_C270_T__0: Option[WLBeam] = findPair(  0, 270,   0)

    val G_90_C_90_T__0: Option[WLBeam] = findPair( 90,  90,   0)
    val G_90_C270_T__0: Option[WLBeam] = findPair( 90, 270,   0)

    val G180_C__0_T__0: Option[WLBeam] = findPair(180,   0,   0)
    val G180_C_90_T__0: Option[WLBeam] = findPair(180,  90,   0)
    val G180_C270_T__0: Option[WLBeam] = findPair(180, 270,   0)

    val G270_C_90_T__0: Option[WLBeam] = findPair(270,  90,   0)
    val G270_C270_T__0: Option[WLBeam] = findPair(270, 270,   0)

    val G180_C270_T_90: Option[WLBeam] = findPair(180, 270,  90)
    val G180_C270_T270: Option[WLBeam] = findPair(180, 270, 270)
    // @formatter:on

    // list of all files required for WL Monthly
    val list = Seq(
      // @formatter:off
      G__0_C_90_T__0,
      G__0_C270_T__0,

      G_90_C_90_T__0,
      G_90_C270_T__0,

      G180_C__0_T__0,
      G180_C_90_T__0,
      G180_C270_T__0,

      G270_C_90_T__0,
      G270_C270_T__0,

      G180_C270_T_90,
      G180_C270_T270
      // @formatter:on
    )

    // if of the files are there then construct the object, otherwise return None.
    if (list.flatten.size == list.size) {
      Some(WLMonthly(
        extendedData,
        // @formatter:off
        G__0_C_90_T__0 = G__0_C_90_T__0.get,
        G__0_C270_T__0 = G__0_C270_T__0.get,

        G_90_C_90_T__0 = G_90_C_90_T__0.get,
        G_90_C270_T__0 = G_90_C270_T__0.get,

        G180_C__0_T__0 = G180_C__0_T__0.get,
        G180_C_90_T__0 = G180_C_90_T__0.get,
        G180_C270_T__0 = G180_C270_T__0.get,

        G270_C_90_T__0 = G270_C_90_T__0.get,
        G270_C270_T__0 = G270_C270_T__0.get,

        G180_C270_T_90 = G180_C270_T_90.get,
        G180_C270_T270 = G180_C270_T270.get
        // @formatter:on
      ))
    }
    else
      None
  }
}
