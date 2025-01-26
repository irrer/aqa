package org.aqa.webrun.wl.isoCheck

/**
  * A point in the 4 dimensional hyperspace being searched.
  *
  * @param Coll_X     dX coordinate
  * @param Coll_Z     dZ coordinate
  * @return
  */
class WLCollimatorPoint(val Coll_X: Double, val Coll_Z: Double) {
  override def toString: String = {

    def fmt(d: Double): String = d.formatted("%21.18f") // prints out full precision of a Double

    s"Coll_X: ${fmt(Coll_X)}    Coll_Z: ${fmt(Coll_Z)}"
  }

}
