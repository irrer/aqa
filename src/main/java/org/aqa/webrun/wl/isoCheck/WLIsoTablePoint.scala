package org.aqa.webrun.wl.isoCheck

/**
  * A point in the 4 dimensional hyperspace being searched.
  *
  * @param dX     dX coordinate
  * @param dZ     dZ coordinate
  * @param isoTableX isoTableX coordinate
  * @param isoTableZ isoTableZ coordinate
  * @return
  */
class WLIsoTablePoint(val dX: Double, val dZ: Double, val isoTableX: Double, val isoTableZ: Double) {
  override def toString: String = {

    def fmt(d: Double): String = d.formatted("%21.18f") // prints out full precision of a Double

    s"dX: ${fmt(dX)}    dZ: ${fmt(dZ)}    TableX: ${fmt(isoTableX)}    TableZ: ${fmt(isoTableZ)}"
  }

}
