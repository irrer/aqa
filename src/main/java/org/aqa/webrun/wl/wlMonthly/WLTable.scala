package org.aqa.webrun.wl.wlMonthly

import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ExtendedData

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
                  ) {

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
  private var dXT__0: Double = 0.183972235121236 // TODO determine proper seed value

  /** Analysis M14 */
  private var dZT__0: Double = 0.332920649471168 // TODO determine proper seed value

  /** Analysis L14 */
  var Table_X: Double = 0.332018792527814 // TODO determine proper seed value

  /** Analysis M14 */
  var Table_Z: Double = 0.323882986957228 // TODO determine proper seed value

  /** Analysis L */
  private def dXOf(beam: WLBeam, dX: Double = dXT__0, dZ: Double = dZT__0): Double = {
    (dX * beam.cos) + (dZ * beam.sin)
  }

  /** Analysis M */
  private def dZOf(beam: WLBeam, dX: Double = dXT__0, dZ: Double = dZT__0): Double = {
    (dZ * beam.cos) - (dX * beam.sin)
  }

  /** Analysis H */
  def BB_X(beam: WLBeam): Double = beam.wl.errorX_mm - T__0.get.wl.errorX_mm // H

  /** Analysis I */
  def BB_Z(beam: WLBeam): Double = beam.wl.errorY_mm - T__0.get.wl.errorY_mm // I

  /** Analysis L */
  def dX(beam: WLBeam): Double = dXOf(beam) // L

  /** Analysis M */
  def dZ(beam: WLBeam): Double = dZOf(beam) // M

  /** Analysis J */
  def BB_Xp(beam: WLBeam): Double = BB_X(beam) + dX(beam) // J

  /** Analysis K */
  def BB_Zp(beam: WLBeam): Double = BB_Z(beam) + dZ(beam)

  /** Analysis P BB-X" */
  def BB_Xpp(beam: WLBeam): Double = BB_Xp(beam) - Table_X

  /** Analysis Q BB-X" */
  def BB_Zpp(beam: WLBeam): Double = BB_Zp(beam) - Table_Z

  /** Analysis P BB-Z" */
  def BB_Rpp(beam: WLBeam): Double = {
    val x = BB_Xpp(beam)
    val z = BB_Zpp(beam)
    (x * x) + (z * z)
  }

  def K12: Double = {
    val BB_XpList = beamList.map(BB_Xp)
    val BB_ZpList = beamList.map(BB_Zp)

    val xRange = BB_XpList.max - BB_XpList.min
    val zRange = BB_ZpList.max - BB_ZpList.min

    Math.max(xRange, zRange)
  }

  def maxSquareOfBBDisplacement: Double = beamList.map(BB_Rpp).max


  def minMax(dXv: Double, dZv: Double, Table_Xv: Double, Table_Zv: Double): Double = {
    0.0 // TODO
  }

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
