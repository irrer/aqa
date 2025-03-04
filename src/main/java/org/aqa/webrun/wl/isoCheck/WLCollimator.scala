package org.aqa.webrun.wl.isoCheck

import org.aqa.Logging
import org.aqa.db.WinstonLutz

case class WLCollimator(
    // @formatter:off
    T__0: WinstonLutz,
    T_90: WinstonLutz,
    T270: WinstonLutz,
    // @formatter:on
                       ) extends Logging {

  private val beamList = Seq(
    T__0,
    T_90,
    T270,
  )

  // ------------------------------------------------------------------------------------------

  private var Coll_X_Optimized: Option[Double] = None

  def get_Coll_X_Optimized: Double = {
    if (Coll_X_Optimized.isEmpty)
      optimizeIfNecessary()
    Coll_X_Optimized.get
  }

  def set_Coll_X_Optimized(coll_x: Double): Unit = Coll_X_Optimized = Some(coll_x)

  // ------------------------------------------------------------------------------------------

  private var Coll_Z_Optimized: Option[Double] = None

  def get_Coll_Z_Optimized: Double = {
    if (Coll_Z_Optimized.isEmpty)
      optimizeIfNecessary()
    Coll_Z_Optimized.get
  }

  def set_Coll_Z_Optimized(coll_z: Double): Unit = Coll_Z_Optimized = Some(coll_z)

  // ------------------------------------------------------------------------------------------

  private var CA_Rpp_Optimized: Option[Double] = None

  def get_CA_Rpp_Optimized: Double = {
    if (CA_Rpp_Optimized.isEmpty)
      optimizeIfNecessary()
    CA_Rpp_Optimized.get
  }

  def set_CA_Rpp_Optimized(r: Double): Unit = CA_Rpp_Optimized = Some(r)

  // ------------------------------------------------------------------------------------------

  def CA_X(wl: WinstonLutz): Double = -wl.errorX_mm

  def CA_Z(wl: WinstonLutz): Double = -wl.errorY_mm

  def CA_Xpp(wl: WinstonLutz, Coll_X: Double): Double = CA_X(wl) - Coll_X

  def CA_Zpp(wl: WinstonLutz, Coll_Z: Double): Double = CA_Z(wl) - Coll_Z

  def CA_Rpp(wl: WinstonLutz, Coll_X: Double, Coll_Z: Double): Double = {
    val x = CA_Xpp(wl, Coll_X)
    val z = CA_Zpp(wl, Coll_Z)
    Math.sqrt((x * x) + (z * z))
  }

  /** Collimator L2 */
  def MinCA_Rpp(Coll_X: Double, Coll_Z: Double): Double = {
    beamList.map(beam => CA_Rpp(beam, Coll_X, Coll_Z)).max
  }


  /**
   * Perform gradient descent to find the best point.
   */
  private def optimize(): Unit = {
    val optimizedPoint = new WLCollimatorGradientDescent(this).findMin()

    set_CA_Rpp_Optimized(MinCA_Rpp(optimizedPoint.Coll_X, optimizedPoint.Coll_Z))

    set_Coll_X_Optimized(optimizedPoint.Coll_X)
    set_Coll_Z_Optimized(optimizedPoint.Coll_Z)
  }

  private def optimizeIfNecessary(): Unit = {
    if (Coll_X_Optimized.isEmpty)
      optimize()
  }

}

object WLCollimator {

  /**
   * Determine if all the data is present to construct a WL IsoTable data set.  If so, make one and return it.
   *
   * @param wlList List of incoming DICOM and results.
   * @return IsoTable data set or None.
   */
  def make(wlMap: WLMap): Option[WLCollimator] = {


    // @formatter:off
    val T__0 : Option[WinstonLutz] = wlMap.find( 180,   0,  0 )
    val T_90 : Option[WinstonLutz] = wlMap.find( 180,  90,  0 )
    val T270 : Option[WinstonLutz] = wlMap.find( 180, 270,  0 )
    // @formatter:on

    // list of all files required for WL IsoTable
    val requiredList: Seq[Option[WinstonLutz]] = Seq(
      T__0,
      T_90,
      T270,
    )

    // if of the files are there then construct the object, otherwise return None.
    if (requiredList.flatten.size == requiredList.size) {
      Some(WLCollimator(
        T__0.get,
        T_90.get,
        T270.get
      ))
    }
    else
      None
  }

}
