package org.aqa.webrun.wl.isoCheck

import org.aqa.webrun.ExtendedData
import org.aqa.Logging

/**
 * Contain and process WL isoCheck data.
 *
 * The naming convention is intentionally pedantic as an attempt to clarify the beams' specifications.
 *
 * The G, C, T abbreviations are for Gantry, Collimator, and Table (couch) angles.
 *
 * @param G__0_C_90_caX X
 * @param G__0_C_90_caZ Z
 * @param G__0_C270_caX X
 * @param G__0_C270_caZ Z
 * @param G_90_C_90_caY X
 * @param G_90_C_90_caZ Z
 * @param G_90_C270_caY X
 * @param G_90_C270_caZ Z
 * @param G180_C__0_caX X
 * @param G180_C__0_caZ Z
 * @param G180_C_90_caX X
 * @param G180_C_90_caZ Z
 * @param G180_C270_caX X
 * @param G180_C270_caZ Z
 * @param G270_C_90_caY X
 * @param G270_C_90_caZ Z
 * @param G270_C270_caY X
 * @param G270_C270_caZ Z
 */
case class IsoCheck(
                     // @formatter:off
                     G__0_C_90_caX: Double, G__0_C_90_caZ: Double,
                     G__0_C270_caX: Double, G__0_C270_caZ: Double,
                     G_90_C_90_caY: Double, G_90_C_90_caZ: Double,
                     G_90_C270_caY: Double, G_90_C270_caZ: Double,
                     G180_C__0_caX: Double, G180_C__0_caZ: Double,
                     G180_C_90_caX: Double, G180_C_90_caZ: Double,
                     G180_C270_caX: Double, G180_C270_caZ: Double,
                     G270_C_90_caY: Double, G270_C_90_caZ: Double,
                     G270_C270_caY: Double, G270_C270_caZ: Double
                     // @formatter:on
                   ) extends Logging {

  /** Analysis I3 = (F3 + F4) / 2 */
  val collXG__0: Double = (G__0_C_90_caX + G__0_C270_caX) / 2

  /** Analysis I8 = (F8 + F9) / 2 */
  val collXG180: Double = (G180_C_90_caX + G180_C270_caX) / 2
  private val collXSeq: Seq[Double] = Seq(collXG__0, collXG180)

  /** Analysis J5 = (G5 + G6) / 2 */
  val collYG_90: Double = (G_90_C_90_caY + G_90_C270_caY) / 2

  /** Analysis J10 = (G10 + G11) / 2 */
  val collYG270: Double = (G270_C_90_caY + G270_C270_caY) / 2
  private val collYSeq: Seq[Double] = Seq(collYG_90, collYG270)

  /** Analysis K3 = (H3 + H4) / 2 */
  val collZG__0: Double = (G__0_C_90_caZ + G__0_C270_caZ) / 2

  /** Analysis K5 = (H5 + H6) / 2 */
  val collZG_90: Double = (G_90_C_90_caZ + G_90_C270_caZ) / 2

  /** Analysis K8 = (H8 + H9) / 2 */
  val collZG180: Double = (G180_C_90_caZ + G180_C270_caZ) / 2

  /** Analysis K10 = (H10 + H11) / 2 */
  val collZG270: Double = (G270_C_90_caZ + G270_C270_caZ) / 2
  private val collZSeq = Seq(collZG__0, collZG_90, collZG180, collZG270)

  /** Analysis L3 = (I3 + I8) / 2 */
  val isoX: Double = collXSeq.sum / collXSeq.size // Analysis L3
  /** Analysis M3 = (J5 + J10) / 2 */
  val isoY: Double = collYSeq.sum / collYSeq.size // Analysis M3
  /** Analysis N3 = (K3 + K5 + K8 + K10) / 4 */
  val isoZ: Double = collZSeq.sum / collZSeq.size // Analysis N3

  /** Analysis M7 = MAX(I3,I7) - MIN(I3,I7) */
  val isoXRange: Double = collXSeq.max - collXSeq.min

  /** Analysis N7 = MAX(J5,J10) - MIN(J5,J10) */
  val isoYRange: Double = collYSeq.max - collYSeq.min

  /** Analysis O7 = MAX(K3,K5,K8,K10) - MIN(K3,K5,K8,K10) */
  val isoZRange: Double = collZSeq.max - collZSeq.min

  /** Analysis O3 = K3 - K8 */
  val gantryFlex: Double = collZG__0 - collZG180

  /** Analysis O8 = SQRT( M7*M7 + N7*N7 + O7*O7) / 2 */
  val gantryIsocentricity: Double = {
    val coordinates = Seq(isoXRange, isoYRange, isoZRange)
    val sumSquares = coordinates.map(v => v * v).sum
    val distance = Math.sqrt(sumSquares)
    val O8 = distance / 2
    O8
  }

  /**
   * Analysis P3  spreadsheet coordinates
   * Analysis P3 ((I3-L3) + (J5-M3) + (L3-I8) + (M3-J10)) / 4
   */
  val collGantryMisalign: Double = Seq(collXG__0 - isoX, collYG_90 - isoY, isoX - collXG180, isoY - collYG270).sum / 4

  // ------------------------------------------------------------------------------------------
  // mlcDx

  /** Analysis Q3 = F3 - I3 */
  val mlcDxG__0_C_90: Double = G__0_C_90_caX - collXG__0

  /** Analysis Q4 = F4 - I3 */
  val mlcDxG__0_C270: Double = G__0_C270_caX - collXG__0

  /** Analysis Q5 = G4 - J5 */
  val mlcDxG_90_C_90: Double = G_90_C_90_caY - collYG_90

  /** Analysis Q6 = G6 - J5 */
  val mlcDxG_90_C270: Double = G_90_C270_caY - collYG_90

  /** Analysis Q7 = -(F7 - I8) */
  val mlcDxG180_C__0: Double = -(G180_C__0_caX - collXG180)

  /** Analysis Q8 = -(F8 - I8) */
  val mlcDxG180_C_90: Double = -(G180_C_90_caX - collXG180)

  /** Analysis Q9 = -(F9 - I8) */
  val mlcDxG180_C270: Double = -(G180_C270_caX - collXG180)

  /** Analysis Analysis Q10 = G10 - J10 */
  val mlcDxG270_C_90: Double = -(G270_C_90_caY - collYG270)

  /** Analysis Analysis Q11 = G11 - J10 */
  val mlcDxG270_C270: Double = -(G270_C270_caY - collYG270)

  // ------------------------------------------------------------------------------------------
  // mlcDz

  /** Analysis R3 = F3 - I3 */
  val mlcDyG__0_C_90: Double = -(G__0_C_90_caZ - collZG__0)

  /** Analysis R4 = F4 - I3 */
  val mlcDyG__0_C270: Double = -(G__0_C270_caZ - collZG__0)

  /** Analysis R5 = H5 - K5 */
  val mlcDyG_90_C_90: Double = -(G_90_C_90_caZ - collZG_90)

  /** Analysis R6 = H6 - K5 */
  val mlcDyG_90_C270: Double = -(G_90_C270_caZ - collZG_90)

  /** Analysis R7 = H7 - K8 */
  val mlcDyG180_C__0: Double = -(G180_C__0_caZ - collZG180)

  /** Analysis R8 = H8 - K8 */
  val mlcDyG180_C_90: Double = -(G180_C_90_caZ - collZG180)

  /** Analysis R9 = H9 - K8 */
  val mlcDyG180_C270: Double = -(G180_C270_caZ - collZG180)

  /** Analysis R10 = H10 - K10 */
  val mlcDyG270_C_90: Double = -(G270_C_90_caZ - collZG270)

  /** Analysis R11 = H11 - K10 */
  val mlcDyG270_C270: Double = -(G270_C270_caZ - collZG270)

  // ------------------------------------------------------------------------------------------

  /** Analysis S3 = (Q3 + Q5 + Q8 + Q10) / 4 */
  val mlcOffsetX_090: Double = (mlcDxG__0_C_90 + mlcDxG_90_C_90 + mlcDxG180_C_90 + mlcDxG270_C_90) / 4

  /** Analysis S4 = (Q4 + Q6 + Q9 + Q11) / 4 */
  val mlcOffsetX_270: Double = (mlcDxG__0_C270 + mlcDxG_90_C270 + mlcDxG180_C270 + mlcDxG270_C270) / 4

  /** Analysis S2 = S3 - S4 */
  val mlcOffsetX: Double = mlcOffsetX_090 - mlcOffsetX_270

  /** Analysis T3 = (R3 + R5 + R8 + R10) / 4 */
  val mlcOffsetY_090: Double = (mlcDyG__0_C_90 + mlcDyG_90_C_90 + mlcDyG180_C_90 + mlcDyG270_C_90) / 4 // Analysis T3
  /** Analysis T4 = (R4 + R6 + R9 + R11) / 4 */
  val mlcOffsetY_270: Double = (mlcDyG__0_C270 + mlcDyG_90_C270 + mlcDyG180_C270 + mlcDyG270_C270) / 4 // Analysis T4
  /** Analysis T2 = T3 - T4 */
  val mlcOffsetY: Double = mlcOffsetY_090 - mlcOffsetY_270 // Analysis T3

  // ------------------------------------------------------------------------------------------

}

object IsoCheck extends Logging {

  /**
   * Determine if all the data is present to construct a WL IsoCheck data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param beamList     List of incoming DICOM and results.
   * @return IsoCheck data set or None.
   */
  def make(extendedData: ExtendedData, beamList: Seq[WLBeam]): Option[IsoCheck] = {

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
      G__0_C_90_T__0,
      G__0_C270_T__0,
      //
      G_90_C_90_T__0,
      G_90_C270_T__0,
      //
      G180_C__0_T__0,
      G180_C_90_T__0,
      G180_C270_T__0,
      //
      G270_C_90_T__0,
      G270_C270_T__0
    )

    // if of the files are there then construct the object, otherwise return None.
    if (list.flatten.size == list.size) {
      Some(IsoCheck(
        G__0_C_90_T__0.get.caX.get, G__0_C_90_T__0.get.caZ.get,
        G__0_C270_T__0.get.caX.get, G__0_C270_T__0.get.caZ.get,
        G_90_C_90_T__0.get.caY.get, G_90_C_90_T__0.get.caZ.get,
        G_90_C270_T__0.get.caY.get, G_90_C270_T__0.get.caZ.get,
        G180_C__0_T__0.get.caX.get, G180_C__0_T__0.get.caZ.get,
        G180_C_90_T__0.get.caX.get, G180_C_90_T__0.get.caZ.get,
        G180_C270_T__0.get.caX.get, G180_C270_T__0.get.caZ.get,
        G270_C_90_T__0.get.caY.get, G270_C_90_T__0.get.caZ.get,
        G270_C270_T__0.get.caY.get, G270_C270_T__0.get.caZ.get,
      ))
    }
    else
      None
  }
}
