package org.aqa.webrun.wl.isoCheck

import org.aqa.Logging

case class WLIsoTable(
                       // @formatter:off
                    T__0: Option[ WLBeam],
                    T_30: Option[ WLBeam],
                    T_60: Option[ WLBeam],
                    T_90: Option[ WLBeam],
                    T270: Option[ WLBeam],
                    T300: Option[ WLBeam],
                    T330: Option[ WLBeam]
                    // @formatter:on
                     ) extends Logging {

  val beamList: Seq[WLBeam] = Seq(
    T__0,
    T_30,
    T_60,
    T_90,
    T270,
    T300,
    T330,
  ).flatten

  /** Analysis L14 */
  private var dXT__0_Optimized: Double = 0.0

  def get_dXT__0_Optimized: Double = dXT__0_Optimized

  def set_dXT__0_Optimized(x: Double): Unit = dXT__0_Optimized = x

  /** Analysis M14 */
  private var dZT__0_Optimized: Double = 0.0

  def get_dZT__0_Optimized: Double = dZT__0_Optimized

  def set_dZT__0_Optimized(z: Double): Unit = dZT__0_Optimized = z

  /** Analysis L14 */
  private var IsoTable_X_Optimized: Double = 0.0

  def get_IsoTable_X_Optimized: Double = IsoTable_X_Optimized

  def set_IsoTable_X_Optimized(x: Double): Unit = IsoTable_X_Optimized = x

  /** Analysis M14 */
  private var IsoTable_Z_Optimized: Double = 0.0

  def get_IsoTable_Z_Optimized: Double = IsoTable_Z_Optimized

  def set_IsoTable_Z_Optimized(z: Double): Unit = IsoTable_Z_Optimized = z

  private var RSquared_Optimized: Double = 0.0

  def get_RSquared_Optimized: Double = RSquared_Optimized

  /** Analysis L */
  def dXOf(beam: WLBeam, dX: Double, dZ: Double): Double = {
    (dX * beam.cos) + (dZ * beam.sin)
  }

  /** Analysis M */
  def dZOf(beam: WLBeam, dX: Double, dZ: Double): Double = {
    (dZ * beam.cos) - (dX * beam.sin)
  }

  /** Analysis H */
  def BB_X(beam: WLBeam): Double = { // H
    val bb_x = beam.wl.errorX_mm - T__0.get.wl.errorX_mm
    bb_x
  }

  /** Analysis I */
  def BB_Z(beam: WLBeam): Double = beam.wl.errorY_mm - T__0.get.wl.errorY_mm // I

  /** Analysis J */
  def BB_Xp(beam: WLBeam, dX: Double, dZ: Double): Double = BB_X(beam) + dXOf(beam, dX, dZ) // J

  /** Analysis K */
  def BB_Zp(beam: WLBeam, dX: Double, dZ: Double): Double = BB_Z(beam) + dZOf(beam, dX, dZ) // K

  /** Analysis P BB-X" */
  private def BB_Xpp(beam: WLBeam, dX: Double, dZ: Double, IsoTable_X: Double): Double = BB_Xp(beam, dX, dZ) - IsoTable_X

  /** Analysis Q BB-X" */
  def BB_Zpp(beam: WLBeam, dX: Double, dZ: Double, IsoTable_Z: Double): Double = BB_Zp(beam, dX, dZ) - IsoTable_Z

  /** Analysis P BB-Z" */
  def BB_Rpp(beam: WLBeam, dX: Double, dZ: Double, IsoTable_X: Double, IsoTable_Z: Double): Double = {
    val x = BB_Xpp(beam, dX, dZ, IsoTable_X)
    val z = BB_Zpp(beam, dX, dZ, IsoTable_Z)
    (x * x) + (z * z)
  }

  def K12(dX: Double, dZ: Double): Double = {
    val BB_XpList = beamList.map(beam => BB_Xp(beam, dX, dZ))
    val BB_ZpList = beamList.map(beam => BB_Zp(beam, dX, dZ))

    val xRange = BB_XpList.max - BB_XpList.min
    val zRange = BB_ZpList.max - BB_ZpList.min

    Math.max(xRange, zRange)
  }

  /**
   * Calculate the minimum R-squared value.  This is spreadsheet cell Analysis R12.
   *
   * @param dX         dX
   * @param dZ         dZ
   * @param IsoTable_X IsoTable_X
   * @param IsoTable_Z IsoTable_Z
   * @return Minimum R**2
   */
  def minSquareOfBBDisplacement(dX: Double, dZ: Double, IsoTable_X: Double, IsoTable_Z: Double): Double = beamList.map(beam => BB_Rpp(beam, dX, dZ, IsoTable_X, IsoTable_Z)).max

  /**
   * Optimize the minimum of the maximum R-squared values using gradient descent.
   */
  private def optimizeRSquared(): Unit = {

    val optimizedPoint = new WLIsoTableGradientDescent(this).findMin()
    RSquared_Optimized = minSquareOfBBDisplacement(optimizedPoint.dX, optimizedPoint.dZ, optimizedPoint.isoTableX, optimizedPoint.isoTableZ)

    dXT__0_Optimized = optimizedPoint.dX
    dZT__0_Optimized = optimizedPoint.dZ
    IsoTable_X_Optimized = optimizedPoint.isoTableX
    IsoTable_Z_Optimized = optimizedPoint.isoTableZ
  }

  // Perform optimization
  optimizeRSquared()
}

object WLIsoTable {

  /**
   * Determine if all the data is present to construct a WL IsoTable data set.  If so, make one and return it.
   *
   * @param pairList List of incoming DICOM and results.
   * @return IsoTable data set or None.
   */
  def make(pairList: Seq[WLBeam]): Option[WLIsoTable] = {

    def findPair(g: Int, c: Int, t: Int): Option[WLBeam] = {
      WLBeam.findGCT(pairList, g, c, t)
    }


    // @formatter:off
    val T__0 : Option[WLBeam] = findPair( 180, 270,   0 )
    val T_30 : Option[WLBeam] = findPair( 180, 270,  30 )
    val T_60 : Option[WLBeam] = findPair( 180, 270,  60 )
    val T_90 : Option[WLBeam] = findPair( 180, 270,  90 )
    val T270 : Option[WLBeam] = findPair( 180, 270, 270 )
    val T300 : Option[WLBeam] = findPair( 180, 270, 300 )
    val T330 : Option[WLBeam] = findPair( 180, 270, 330 )
    // @formatter:on

    // list of all files required for WL IsoTable
    val requiredList = Seq(
      T__0,
      T_90,
      T270,
    )

    // if of the files are there then construct the object, otherwise return None.
    if (requiredList.flatten.size == requiredList.size) {
      Some(WLIsoTable(
        T__0,
        T_30,
        T_60,
        T_90,
        T270,
        T300,
        T330
      ))
    }
    else
      None
  }

  def CA_X(beam: WLBeam): Double = -beam.wl.errorX_mm

  def CA_Z(beam: WLBeam): Double = -beam.wl.errorY_mm

}
