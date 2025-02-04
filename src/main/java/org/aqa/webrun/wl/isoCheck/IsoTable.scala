package org.aqa.webrun.wl.isoCheck

import org.aqa.webrun.ExtendedData
import org.aqa.Logging

case class IsoTable(
                     // @formatter:off
                    extendedData: ExtendedData,
                    T__0_CA_X: Option[Double], T__0_CA_Z: Option[Double],
                    T_30_CA_X: Option[Double], T_30_CA_Z: Option[Double],
                    T_60_CA_X: Option[Double], T_60_CA_Z: Option[Double],
                    T_90_CA_X: Option[Double], T_90_CA_Z: Option[Double],
                    T270_CA_X: Option[Double], T270_CA_Z: Option[Double],
                    T300_CA_X: Option[Double], T300_CA_Z: Option[Double],
                    T330_CA_X: Option[Double], T330_CA_Z: Option[Double]
                    // @formatter:on
                   ) extends Logging {


  private def makeTBeam(gantryAngle: Int, collimatorAngle: Int, tableAngle: Int, x: Option[Double], z: Option[Double]): Option[TableBeam] = {
    if (x.isDefined && z.isDefined)
      Some(TableBeam(gantryAngle: Int, collimatorAngle: Int, tableAngle: Int, x.get, z.get))
    else
      None
  }

  val T__0: Option[TableBeam] = makeTBeam(180, 270, 0, T__0_CA_X, T__0_CA_Z)
  val T_30: Option[TableBeam] = makeTBeam(180, 270, 30, T_30_CA_X, T_30_CA_Z)
  val T_60: Option[TableBeam] = makeTBeam(180, 270, 60, T_60_CA_X, T_60_CA_Z)
  val T_90: Option[TableBeam] = makeTBeam(180, 270, 90, T_90_CA_X, T_90_CA_Z)
  val T270: Option[TableBeam] = makeTBeam(180, 270, 270, T270_CA_X, T270_CA_Z)
  val T300: Option[TableBeam] = makeTBeam(180, 270, 300, T300_CA_X, T300_CA_Z)
  val T330: Option[TableBeam] = makeTBeam(180, 270, 330, T330_CA_X, T330_CA_Z)


  private val TBeamList = Seq(
    T__0,
    T_30,
    T_60,
    T_90,
    T270,
    T300,
    T330
  ).flatten


  /** Analysis L14 */
  private var dXT__0_Optimized: Double = 0.0

  def get_dXT__0_Optimized: Double = dXT__0_Optimized

  /** Analysis M14 */
  private var dZT__0_Optimized: Double = 0.0

  def get_dZT__0_Optimized: Double = dZT__0_Optimized

  /** Analysis L14 */
  private var IsoTable_X_Optimized: Double = 0.0

  def get_IsoTable_X_Optimized: Double = IsoTable_X_Optimized

  /** Analysis M14 */
  private var IsoTable_Z_Optimized: Double = 0.0

  def get_IsoTable_Z_Optimized: Double = IsoTable_Z_Optimized

  private var RSquared_Optimized: Double = 0.0

  def get_RSquared_Optimized: Double = RSquared_Optimized

  /** Analysis L */
  def dXOf(beam: TableBeam, dX: Double, dZ: Double): Double = {
    (dX * beam.cos) + (dZ * beam.sin)
  }

  /** Analysis M */
  def dZOf(beam: TableBeam, dX: Double, dZ: Double): Double = {
    (dZ * beam.cos) - (dX * beam.sin)
  }

  /** Analysis H */
  def BB_X(beam: TableBeam): Double = T__0.get.caX - beam.caX // H

  /** Analysis I */
  def BB_Z(beam: TableBeam): Double = T__0.get.caZ - beam.caZ // I

  /** Analysis J */
  def BB_Xp(beam: TableBeam, dX: Double, dZ: Double): Double = BB_X(beam) + dXOf(beam, dX, dZ) // J

  /** Analysis K */
  def BB_Zp(beam: TableBeam, dX: Double, dZ: Double): Double = BB_Z(beam) + dZOf(beam, dX, dZ) // K

  /** Analysis P BB-X" */
  private def BB_Xpp(beam: TableBeam, dX: Double, dZ: Double, IsoTable_X: Double): Double = BB_Xp(beam, dX, dZ) - IsoTable_X

  /** Analysis Q BB-X" */
  def BB_Zpp(beam: TableBeam, dX: Double, dZ: Double, IsoTable_Z: Double): Double = BB_Zp(beam, dX, dZ) - IsoTable_Z

  /** Analysis P BB-Z" */
  def BB_Rpp(beam: TableBeam, dX: Double, dZ: Double, IsoTable_X: Double, IsoTable_Z: Double): Double = {
    val x = BB_Xpp(beam, dX, dZ, IsoTable_X)
    val z = BB_Zpp(beam, dX, dZ, IsoTable_Z)
    (x * x) + (z * z)
  }

  def K12(dX: Double, dZ: Double): Double = {
    val BB_XpList = TBeamList.map(beam => BB_Xp(beam, dX, dZ))
    val BB_ZpList = TBeamList.map(beam => BB_Zp(beam, dX, dZ))

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
  def minSquareOfBBDisplacement(dX: Double, dZ: Double, IsoTable_X: Double, IsoTable_Z: Double): Double = TBeamList.map(beam => BB_Rpp(beam, dX, dZ, IsoTable_X, IsoTable_Z)).max

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

object IsoTable {

  /**
   * Determine if all the data is present to construct a WL IsoTable data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param pairList     List of incoming DICOM and results.
   * @return IsoTable data set or None.
   */
  def make(extendedData: ExtendedData, pairList: Seq[WLBeam]): Option[IsoTable] = {

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
      Some(IsoTable(extendedData,
        T__0.get.caX, T__0.get.caZ,
        T_30.get.caX, T_30.get.caZ,
        T_60.get.caX, T_60.get.caZ,
        T_90.get.caX, T_90.get.caZ,
        T270.get.caX, T270.get.caZ,
        T300.get.caX, T300.get.caZ,
        T330.get.caX, T330.get.caZ
      ))
    }
    else
      None
  }

  def CA_X(beam: WLBeam): Double = -beam.wl.errorX_mm

  def CA_Z(beam: WLBeam): Double = -beam.wl.errorY_mm


}
