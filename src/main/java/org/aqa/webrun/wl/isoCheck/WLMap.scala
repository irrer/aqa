package org.aqa.webrun.wl.isoCheck

import org.aqa.db.WinstonLutz

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
class WLMap(fullList: Seq[WinstonLutz]) {
  private def nameOf(g: Int, c: Int, t: Int): String = s"G$g C$c t$t"
  private def nameOf(wl: WinstonLutz): String = nameOf(wl.gantryAngleRounded, wl.collimatorAngleRounded, wl.tableAngleRounded.get)
  val list: Seq[WinstonLutz] = {
    // list of groups that the same gantry, collimator, and table angles
    val groupList = fullList.filter(_.tableAngleRounded.isDefined).groupBy(nameOf).values

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
  def find(gantry: Int, collimator: Int, table: Int): Option[WinstonLutz] = wlMap.get(nameOf(gantry, collimator, table))

}
