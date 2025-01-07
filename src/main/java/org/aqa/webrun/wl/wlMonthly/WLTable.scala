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

  /** Analysis L14 */
  var dXT__0: Double = 0.183972235121236 // TODO determine proper seed value

  /** Analysis M14 */
  var dZT__0: Double = 0.332920649471168 // TODO determine proper seed value

  /** Analysis L14 */
  var Table_X: Double = 0.332018792527814 // TODO determine proper seed value

  /** Analysis M14 */
  var Table_Z: Double = 0.323882986957228 // TODO determine proper seed value


  /** Analysis L */
  def dXOf(beam: WLBeam, dX: Double = dXT__0, dZ: Double = dZT__0): Double = {
    (dX * beam.cos) + (dZ * beam.sin)
  }

  /** Analysis M */
  def dZOf(beam: WLBeam, dX: Double = dXT__0, dZ: Double = dZT__0): Double = {
    (dZ * beam.cos) - (dX * beam.sin)
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

    val machine = extendedData.machine

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
}
