package org.aqa.webrun.wl.isoCheck

import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ExtendedData
import org.aqa.Logging
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.rnd

/**
  * Contain and process WL isoCheck data.
  *
  * The naming convention is intentionally pedantic as an attempt to clarify the beams' specifications.
  *
  * The G, C, T abbreviations are for Gantry, Collimator, and IsoTable angles.
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
case class WLIsoCheck( // @formatter:off
                      extendedData: ExtendedData,
                      G__0_C_90_T__0: WLBeam,
                      G__0_C270_T__0: WLBeam,

                      G_90_C_90_T__0: WLBeam,
                      G_90_C270_T__0: WLBeam,

                      G180_C__0_T__0: WLBeam,
                      G180_C_90_T__0: WLBeam,
                      G180_C270_T__0: WLBeam,

                      G270_C_90_T__0: WLBeam,
                      G270_C270_T__0: WLBeam
                      // @formatter:on
                     ) extends Logging {

  /** Analysis I3 = (F3 + F4) / 2 */
  val collXG__0: Double = rnd((G__0_C_90_T__0.caX.get + G__0_C270_T__0.caX.get) / 2)
  /** Analysis I8 = (F8 + F9) / 2 */
  val collXG180: Double = rnd((G180_C_90_T__0.caX.get + G180_C270_T__0.caX.get) / 2)
  private val collXSeq: Seq[Double] = Seq(collXG__0, collXG180)

  /** Analysis J5 = (G5 + G6) / 2 */
  val collYG_90: Double = rnd((G_90_C_90_T__0.caY.get + G_90_C270_T__0.caY.get) / 2)
  /** Analysis J10 = (G10 + G11) / 2 */
  val collYG270: Double = rnd((G270_C_90_T__0.caY.get + G270_C270_T__0.caY.get) / 2)
  private val collYSeq: Seq[Double] = Seq(collYG_90, collYG270)

  /** Analysis K3 = (H3 + H4) / 2 */
  val collZG__0: Double = rnd((G__0_C_90_T__0.caZ.get + G__0_C270_T__0.caZ.get) / 2)
  /** Analysis K5 = (H5 + H6) / 2 */
  val collZG_90: Double = rnd((G_90_C_90_T__0.caZ.get + G_90_C270_T__0.caZ.get) / 2)
  /** Analysis K8 = (H8 + H9) / 2 */
  val collZG180: Double = rnd((G180_C_90_T__0.caZ.get + G180_C270_T__0.caZ.get) / 2)
  /** Analysis K10 = (H10 + H11) / 2 */
  val collZG270: Double = rnd((G270_C_90_T__0.caZ.get + G270_C270_T__0.caZ.get) / 2)
  private val collZSeq = Seq(collZG__0, collZG_90, collZG180, collZG270)

  /** Analysis L3 = (I3 + I8) / 2 */
  val isoX: Double = rnd(collXSeq.sum / collXSeq.size) // Analysis L3
  /** Analysis M3 = (J5 + J10) / 2 */
  val isoY: Double = rnd(collYSeq.sum / collYSeq.size) // Analysis M3
  /** Analysis N3 = (K3 + K5 + K8 + K10) / 4 */
  val isoZ: Double = rnd(collZSeq.sum / collZSeq.size) // Analysis N3


  /** Analysis M7 = MAX(I3,I7) - MIN(I3,I7) */
  val isoXRange: Double = rnd(collXSeq.max - collXSeq.min)
  /** Analysis N7 = MAX(J5,J10) - MIN(J5,J10) */
  val isoYRange: Double = rnd(collYSeq.max - collYSeq.min)
  /** Analysis O7 = MAX(K3,K5,K8,K10) - MIN(K3,K5,K8,K10) */
  val isoZRange: Double = rnd(collZSeq.max - collZSeq.min)

  /** Analysis O3 = K3 - K8 */
  val gantryFlex: Double = rnd(collZG__0 - collZG180)

  /** Analysis O8 = SQRT( M7*M7 + N7*N7 + O7*O7) / 2 */
  val gantryIsocentricity: Double = {
    val coordinates = Seq(isoXRange, isoYRange, isoZRange)
    val sumSquares = coordinates.map(v => v * v).sum
    val distance = Math.sqrt(sumSquares)
    val O8 = rnd(distance / 2)
    O8
  }

  /**
   * Analysis P3  spreadsheet coordinates
   * Analysis P3 ((I3-L3) + (J5-M3) + (L3-I8) + (M3-J10)) / 4
   */
  val collGantryMisalign: Double = rnd(Seq(collXG__0 - isoX, collYG_90 - isoY, isoX - collXG180, isoY - collYG270).sum / 4)

  // ------------------------------------------------------------------------------------------
  // mlcDx

  /** Analysis Q3 = F3 - I3 */
  val mlcDxG__0_C_90: Double = rnd(G__0_C_90_T__0.caX.get - collXG__0)
  /** Analysis Q4 = F4 - I3 */
  val mlcDxG__0_C270: Double = rnd(G__0_C270_T__0.caX.get - collXG__0)


  /** Analysis Q5 = G4 - J5 */
  val mlcDxG_90_C_90: Double = rnd(G_90_C_90_T__0.caY.get - collYG_90)
  /** Analysis Q6 = G6 - J5 */
  val mlcDxG_90_C270: Double = rnd(G_90_C270_T__0.caY.get - collYG_90)


  /** Analysis Q7 = -(F7 - I8) */
  val mlcDxG180_C__0: Double = -rnd(G180_C__0_T__0.caX.get - collXG180)
  /** Analysis Q8 = -(F8 - I8) */
  val mlcDxG180_C_90: Double = -rnd(G180_C_90_T__0.caX.get - collXG180)
  /** Analysis Q9 = -(F9 - I8) */
  val mlcDxG180_C270: Double = -rnd(G180_C270_T__0.caX.get - collXG180)

  /** Analysis Analysis Q10 = G10 - J10 */
  val mlcDxG270_C_90: Double = -rnd(G270_C_90_T__0.caY.get - collYG270)
  /** Analysis Analysis Q11 = G11 - J10 */
  val mlcDxG270_C270: Double = -rnd(G270_C270_T__0.caY.get - collYG270)

  // ------------------------------------------------------------------------------------------
  // mlcDz

  /** Analysis R3 = F3 - I3 */
  val mlcDyG__0_C_90: Double = -rnd(G__0_C_90_T__0.caZ.get - collZG__0)
  /** Analysis R4 = F4 - I3 */
  val mlcDyG__0_C270: Double = -rnd(G__0_C270_T__0.caZ.get - collZG__0)

  /** Analysis R5 = H5 - K5 */
  val mlcDyG_90_C_90: Double = -rnd(G_90_C_90_T__0.caZ.get - collZG_90)
  /** Analysis R6 = H6 - K5 */
  val mlcDyG_90_C270: Double = -rnd(G_90_C270_T__0.caZ.get - collZG_90)

  /** Analysis R7 = H7 - K8 */
  val mlcDyG180_C__0: Double = -rnd(G180_C__0_T__0.caZ.get - collZG180)
  /** Analysis R8 = H8 - K8 */
  val mlcDyG180_C_90: Double = -rnd(G180_C_90_T__0.caZ.get - collZG180)
  /** Analysis R9 = H9 - K8 */
  val mlcDyG180_C270: Double = -rnd(G180_C270_T__0.caZ.get - collZG180)

  /** Analysis R10 = H10 - K10 */
  val mlcDyG270_C_90: Double = -rnd(G270_C_90_T__0.caZ.get - collZG270)
  /** Analysis R11 = H11 - K10 */
  val mlcDyG270_C270: Double = -rnd(G270_C270_T__0.caZ.get - collZG270)

  // ------------------------------------------------------------------------------------------

  /** Analysis S3 = (Q3 + Q5 + Q8 + Q10) / 4 */
  val mlcOffsetX_090: Double = rnd((mlcDxG__0_C_90 + mlcDxG_90_C_90 + mlcDxG180_C_90 + mlcDxG270_C_90) / 4)
  /** Analysis S4 = (Q4 + Q6 + Q9 + Q11) / 4 */
  val mlcOffsetX_270: Double = rnd((mlcDxG__0_C270 + mlcDxG_90_C270 + mlcDxG180_C270 + mlcDxG270_C270) / 4)
  /** Analysis S2 = S3 - S4 */
  val mlcOffsetX: Double = rnd(mlcOffsetX_090 - mlcOffsetX_270)

  /** Analysis T3 = (R3 + R5 + R8 + R10) / 4 */
  val mlcOffsetY_090: Double = rnd((mlcDyG__0_C_90 + mlcDyG_90_C_90 + mlcDyG180_C_90 + mlcDyG270_C_90) / 4) // Analysis T3
  /** Analysis T4 = (R4 + R6 + R9 + R11) / 4 */
  val mlcOffsetY_270: Double = rnd((mlcDyG__0_C270 + mlcDyG_90_C270 + mlcDyG180_C270 + mlcDyG270_C270) / 4) // Analysis T4
  /** Analysis T2 = T3 - T4 */
  val mlcOffsetY: Double = rnd(mlcOffsetY_090 - mlcOffsetY_270) // Analysis T3

  // ------------------------------------------------------------------------------------------

  if (true) { // TODO rm
    Trace.trace("G__0_C_90_T__0.wl: " + G__0_C_90_T__0.wl)
    Trace.trace("G__0_C270_T__0.wl: " + G__0_C270_T__0.wl)
    Trace.trace("G180_C_90_T__0.wl: " + G180_C_90_T__0.wl)
    Trace.trace("G180_C270_T__0.wl: " + G180_C270_T__0.wl)

    Trace.trace("G_90_C_90_T__0.wl: " + G__0_C_90_T__0.wl)
    Trace.trace("G_90_C270_T__0.wl: " + G__0_C270_T__0.wl)

    Trace.trace("-----------------------------------------------------------------------------------------------------------")

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

    Trace.trace("gantryFlex: " + gantryFlex)
    Trace.trace("gantryIsocentricity: " + gantryIsocentricity)
    Trace.trace("collGantryMisalign: " + collGantryMisalign)

    Trace.trace("mlcDxG__0_C_90: " + mlcDxG__0_C_90)
    Trace.trace("mlcDxG__0_C270: " + mlcDxG__0_C270)
    Trace.trace("mlcDxG_90_C_90: " + mlcDxG_90_C_90)
    Trace.trace("mlcDxG_90_C270: " + mlcDxG_90_C270)
    Trace.trace("mlcDxG180_C__0: " + mlcDxG180_C__0)
    Trace.trace("mlcDxG180_C_90: " + mlcDxG180_C_90)
    Trace.trace("mlcDxG180_C270: " + mlcDxG180_C270)
    Trace.trace("mlcDxG270_C_90: " + mlcDxG270_C_90)
    Trace.trace("mlcDxG270_C270: " + mlcDxG270_C270)
    Trace.trace("mlcDyG__0_C_90: " + mlcDyG__0_C_90)
    Trace.trace("mlcDyG__0_C270: " + mlcDyG__0_C270)
    Trace.trace("mlcDyG_90_C_90: " + mlcDyG_90_C_90)
    Trace.trace("mlcDyG_90_C270: " + mlcDyG_90_C270)
    Trace.trace("mlcDyG180_C__0: " + mlcDyG180_C__0)
    Trace.trace("mlcDyG180_C_90: " + mlcDyG180_C_90)
    Trace.trace("mlcDyG180_C270: " + mlcDyG180_C270)
    Trace.trace("mlcDyG270_C_90: " + mlcDyG270_C_90)
    Trace.trace("mlcDyG270_C270: " + mlcDyG270_C270)
    Trace.trace("mlcOffsetX_090: " + mlcOffsetX_090)
    Trace.trace("mlcOffsetX_270: " + mlcOffsetX_270)
    Trace.trace("mlcOffsetX: " + mlcOffsetX)
    Trace.trace("mlcOffsetY_090: " + mlcOffsetY_090)
    Trace.trace("mlcOffsetY_270: " + mlcOffsetY_270)
    Trace.trace("mlcOffsetY: " + mlcOffsetY)

    Trace.trace()
  }


}

object WLIsoCheck extends Logging {

  /**
   * Determine if all the data is present to construct a WL IsoCheck data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param beamList     List of incoming DICOM and results.
   * @return IsoCheck data set or None.
   */
  def make(extendedData: ExtendedData, beamList: Seq[WLBeam]): Option[WLIsoCheck] = {

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
    // @formatter:on

    // list of all files required for WL IsoCheck
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
      G270_C270_T__0
      // @formatter:on
    )

    // if of the files are there then construct the object, otherwise return None.
    if (list.flatten.size == list.size) {
      Some(WLIsoCheck(
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
        G270_C270_T__0 = G270_C270_T__0.get
        // @formatter:on
      ))
    }
    else
      None
  }
}
