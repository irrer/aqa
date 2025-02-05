package org.aqa.webrun.wl.isoCheck

import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.flip

case class TableBeam(gantryAngle: Int, collimatorAngle: Int, tableAngle: Int, caX: Double, caZ: Double) {
  def this(wlBeam: WLBeam) = this(wlBeam.gantryAngle, wlBeam.collimatorAngle, wlBeam.tableAngle, wlBeam.caX.get, wlBeam.caZ.get)
  // val bbX: Double = caX - T__0_CA_X.get
  // val bbZ: Double = caZ - T__0_CA_Z.get

  private val radians: Double = Math.toRadians(flip(tableAngle))

  val sin: Double = Math.sin(radians)
  val cos: Double = Math.cos(radians)

}
