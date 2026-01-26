package org.aqa.webrun.wl.isoCheck

import org.aqa.Util
import org.aqa.db.WinstonLutzGeneric

/**
  * Provide support for accessing Winston Lutz results for IsoCheck code.
  *
  * Discards images that have the same gantry, collimator, and table angles by using only the temporally latest one.
  *
  * Provides list of temporally sorted images.
  *
  * Provides function for accessing via gantry, collimator, and table angles.
  *
  * @param fullList All Winston Lutz results.
  */
class WLMap(fullList: Seq[WinstonLutzGeneric]) {

  private def r1 = Util.angleRoundedTo2 _
  private def nameOf(g: Int, c: Int, t: Int): String = s"G$g C$c t$t"
  private def nameOf(wl: WinstonLutzGeneric): String = nameOf( r1(wl.gantryAngle_deg), r1(wl.collimatorAngle_deg), r1(wl.tableAngle_deg.get))
  val list: Seq[WinstonLutzGeneric] = {
    // list of groups that the same gantry, collimator, and table angles
    val groupList = fullList.filter(_.tableAngle_deg.isDefined).groupBy(nameOf).values

    // take only the last one delivered in each group.  Sort the final list by time.
    groupList.map(_.maxBy(_.dataDate.getTime)).toSeq.sortBy(_.dataDate.getTime)
  }

  private val wlMap = list.map(wl => (nameOf(wl), wl)).toMap

  /**
    * Find a beam given its angle values
    * @param gantry Gantry angle.
    * @param collimator Collimator angle.
    * @param table Table angle.
    * @return
    */
  def find(gantry: Int, collimator: Int, table: Int = 0): Option[WinstonLutzGeneric] = wlMap.get(nameOf(gantry, collimator, table))

  def findYaw(gantry: Int, collimator: Int, yaw: Int): Option[WinstonLutzGeneric] = wlMap.get(nameOf(gantry, collimator, Util.negateAngle(yaw)))

}
