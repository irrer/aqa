package org.aqa.webrun.wl.wlMonthly

import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ExtendedData
import org.aqa.Logging

case class WLTable(
    // @formatter:off
                    extendedData: ExtendedData,
                    T__0: Option[ WLBeam],
                    T_30: Option[ WLBeam],
                    T_60: Option[ WLBeam],
                    T_90: Option[ WLBeam],
                    T270: Option[ WLBeam],
                    T300: Option[ WLBeam],
                    T330: Option[ WLBeam]
                    // @formatter:on
                  ) extends Logging{

  private val beamList = Seq(
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

  /** Analysis M14 */
  private var dZT__0_Optimized: Double = 0.0

  def get_dZT__0_Optimized: Double = dZT__0_Optimized

  /** Analysis L14 */
  private var Table_X_Optimized: Double = 0.0

  def get_Table_X_Optimized: Double = Table_X_Optimized

  /** Analysis M14 */
  private var Table_Z_Optimized: Double = 0.0

  def get_Table_Z_Optimized: Double = Table_Z_Optimized

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
  def BB_X(beam: WLBeam): Double = beam.wl.errorX_mm - T__0.get.wl.errorX_mm // H

  /** Analysis I */
  def BB_Z(beam: WLBeam): Double = beam.wl.errorY_mm - T__0.get.wl.errorY_mm // I

  /** Analysis J */
  def BB_Xp(beam: WLBeam, dX: Double, dZ: Double): Double = BB_X(beam) + dXOf(beam, dX, dZ) // J

  /** Analysis K */
  def BB_Zp(beam: WLBeam, dX: Double, dZ: Double): Double = BB_Z(beam) + dZOf(beam, dX, dZ) // K

  /** Analysis P BB-X" */
  def BB_Xpp(beam: WLBeam, dX: Double, dZ: Double, Table_X: Double): Double = BB_Xp(beam, dX, dZ) - Table_X

  /** Analysis Q BB-X" */
  def BB_Zpp(beam: WLBeam, dX: Double, dZ: Double, Table_Z: Double): Double = BB_Zp(beam, dX, dZ) - Table_Z

  /** Analysis P BB-Z" */
  def BB_Rpp(beam: WLBeam, dX: Double, dZ: Double, Table_X: Double, Table_Z: Double): Double = {
    val x = BB_Xpp(beam, dX, dZ, Table_X)
    val z = BB_Zpp(beam, dX, dZ, Table_Z)
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
   * @param dX      dX
   * @param dZ      dZ
   * @param Table_X Table_X
   * @param Table_Z Table_Z
   * @return Minimum R**2
   */
  def minSquareOfBBDisplacement(dX: Double, dZ: Double, Table_X: Double, Table_Z: Double): Double = beamList.map(beam => BB_Rpp(beam, dX, dZ, Table_X, Table_Z)).max

  if (true) { // TODO rm

    val list = beamList.map(beam => BB_Rpp(beam, 0.183972235121236, 0.332920649471168, 0.332018792527814, 0.323882986957228))

    Trace.trace(s"""Min List: \n    ${list.mkString("\n    ")}""")

    val j = minSquareOfBBDisplacement(0.183972235121236, 0.332920649471168, 0.332018792527814, 0.323882986957228)
    Trace.trace(s"R12 min: $j")
    Trace.trace()
    val min = new WLTableGradientDescent(this).findMin()
  }

  /**
   * Optimize the minimum of the maximum R-squared values using gradient descent.
   */
  private def optimizeRSquared(): Unit = {

    val optimizedPoint = new WLTableGradientDescent(this).findMin()
    RSquared_Optimized = minSquareOfBBDisplacement(optimizedPoint.dX, optimizedPoint.dZ, optimizedPoint.tableX, optimizedPoint.tableZ)

    dXT__0_Optimized = optimizedPoint.dX
    dZT__0_Optimized = optimizedPoint.dZ
    Table_X_Optimized = optimizedPoint.tableX
    Table_Z_Optimized = optimizedPoint.tableZ
  }

  // Perform optimization
  optimizeRSquared()

  Trace.trace("T__0: " + T__0.get.wl)
  if (T_30.isDefined) Trace.trace("T_30: " + T_30.get.wl)
  if (T_60.isDefined) Trace.trace("T_60: " + T_60.get.wl)
  Trace.trace("T_90: " + T_90.get.wl)
  Trace.trace("T270: " + T270.get.wl)
  if (T300.isDefined) Trace.trace("T300: " + T300.get.wl)
  if (T330.isDefined) Trace.trace("T330: " + T330.get.wl)
}

object WLTable {

  /**
   * Determine if all the data is present to construct a WL Table data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param pairList     List of incoming DICOM and results.
   * @return Table data set or None.
   */
  def make(extendedData: ExtendedData, pairList: Seq[WLBeam]): Option[WLTable] = {

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

    // list of all files required for WL Table
    val requiredList = Seq(
      T__0,
      T_90,
      T270,
    )

    // if of the files are there then construct the object, otherwise return None.
    if (requiredList.flatten.size == requiredList.size) {
      Some(WLTable(extendedData,
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
