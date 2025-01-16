package org.aqa.webrun.wl.wlMonthly

/**
 * A point in the 4 dimensional hyperspace being searched.
 *
 * @param dX     dX coordinate
 * @param dZ     dZ coordinate
 * @param tableX tableX coordinate
 * @param tableZ tableZ coordinate
 * @return
 */
class WLTablePoint(val dX: Double, val dZ: Double, val tableX: Double, val tableZ: Double) {
  override def toString: String = {

    def fmt(d: Double): String = d.formatted("%30.27f")

    s"dX: ${fmt(dX)}    dZ: ${fmt(dZ)}    tableX: ${fmt(tableX)}    tableZ: ${fmt(tableZ)}"
  }


}