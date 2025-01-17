package org.aqa.webrun.wl.wlMonthly

import org.aqa.webrun.ExtendedData
import org.aqa.Logging

case class WLCollimator(
                         // @formatter:off
                    extendedData: ExtendedData,
                    T__0: WLBeam,
                    T_90: WLBeam,
                    T270: WLBeam,
                    // @formatter:on
                       ) extends Logging {

  private val beamList = Seq(
    T__0,
    T_90,
    T270,
  )

  private var Coll_X_Optimized: Double = -1

  def getColl_X_Optimized: Double = Coll_X_Optimized

  private var Coll_Z_Optimized: Double = -1

  def getColl_Z_Optimized: Double = Coll_Z_Optimized

  private var CA_Rpp_Optimized: Double = -1

  def getCA_Rpp_Optimized: Double = CA_Rpp_Optimized


  def CA_X(beam: WLBeam): Double = -beam.wl.errorX_mm

  def CA_Z(beam: WLBeam): Double = -beam.wl.errorY_mm

  def CA_Xpp(beam: WLBeam, Coll_X: Double): Double = CA_X(beam) - Coll_X

  def CA_Zpp(beam: WLBeam, Coll_Z: Double): Double = CA_Z(beam) - Coll_Z

  def CA_Rpp(beam: WLBeam, Coll_X: Double, Coll_Z: Double): Double = {
    val x = CA_Xpp(beam, Coll_X)
    val z = CA_Zpp(beam, Coll_Z)
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

    CA_Rpp_Optimized = MinCA_Rpp(optimizedPoint.Coll_X, optimizedPoint.Coll_Z)

    Coll_X_Optimized = optimizedPoint.Coll_X
    Coll_Z_Optimized = optimizedPoint.Coll_Z
  }

  // Perform optimization
  optimize()
}

object WLCollimator {

  /**
   * Determine if all the data is present to construct a WL Table data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param pairList     List of incoming DICOM and results.
   * @return Table data set or None.
   */
  def make(extendedData: ExtendedData, pairList: Seq[WLBeam]): Option[WLCollimator] = {

    def findPair(g: Int, c: Int, t: Int): Option[WLBeam] = {
      WLBeam.findGCT(pairList, g, c, t)
    }


    // @formatter:off
    val T__0 : Option[WLBeam] = findPair( 180,   0,  0 )
    val T_90 : Option[WLBeam] = findPair( 180,  90,  0 )
    val T270 : Option[WLBeam] = findPair( 180, 270,  0 )
    // @formatter:on

    // list of all files required for WL Table
    val requiredList: Seq[Option[WLBeam]] = Seq(
      T__0,
      T_90,
      T270,
    )

    // if of the files are there then construct the object, otherwise return None.
    if (requiredList.flatten.size == requiredList.size) {
      Some(WLCollimator(extendedData,
        T__0.get,
        T_90.get,
        T270.get
      ))
    }
    else
      None
  }

}
