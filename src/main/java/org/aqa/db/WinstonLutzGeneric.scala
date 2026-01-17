package org.aqa.db

import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.rnd

trait WinstonLutzGeneric {
  // @formatter:off
  val PK                       : Option[Long]
  val outputPK                 : Long
  val rtimageUID               : String
  val beamName                 : Option[String]
  val gantryAngle_deg          : Double
  val collimatorAngle_deg      : Double
  val tableAngle_deg           : Option[Double]
  val dataDate                 : java.sql.Timestamp
  //
  val boxCenterX_mm            : Double
  val boxCenterY_mm            : Double
  //
  val ballCenterX_mm           : Double
  val ballCenterY_mm           : Double
  //
  val X1Offset_mm              : Option[Double]
  val X2Offset_mm              : Option[Double]
  val Y1Offset_mm              : Option[Double]
  val Y2Offset_mm              : Option[Double]
  //
  val X1Type                   : Option[String]
  val X2Type                   : Option[String]
  val Y1Type                   : Option[String]
  val Y2Type                   : Option[String]
  // @formatter:off

  //noinspection ScalaWeakerAccess
  def errorX_mm: Double = {
    def e = boxCenterX_mm - ballCenterX_mm
    // Trace.trace(s"also from WinstonLutzGeneric beamName: $beamName     boxCenterX_mm: $boxCenterX_mm   ballCenterX_mm: $ballCenterX_mm     e: $e    ")
    boxCenterX_mm - ballCenterX_mm
    e
  }

  //noinspection ScalaWeakerAccess
  def errorY_mm: Double = boxCenterY_mm - ballCenterY_mm

  def errorXY_mm: Double = Math.sqrt((errorX_mm * errorX_mm) + (errorY_mm * errorY_mm))

  { // TODO rm
    def f(d: Double): String = "%10.6f".format(d)
    def bn = "%20s".format(beamName.toString)
    Trace.trace(s"from WinstonLutzGeneric: beamName: $bn boxCenterX_mm: ${f(boxCenterX_mm)}     boxCenterY_mm: ${f(boxCenterY_mm)}     ballCenterX_mm: ${f(ballCenterX_mm)}     ballCenterY_mm: ${f(ballCenterY_mm)}     errorX_mm: ${f(errorX_mm)}     errorY_mm: ${f(errorY_mm)}")

    if (f(errorX_mm).equals("-0.023776"))
      Trace.trace("bad X center")
  }

  def gantryAngleRounded: Int = Util.angleRoundedTo90(gantryAngle_deg)
  def collimatorAngleRounded: Int = Util.angleRoundedTo90(collimatorAngle_deg)
  def tableAngleRounded: Option[Int] = tableAngle_deg.map(WLXlsxUtil.angleRounded)

  def yaw: Option[Int] = tableAngle_deg.map(Util.angleRoundedTo1).map(Util.negateAngle)

  private def rawRadians: Option[Double] = yaw.map(_.toDouble).map(Math.toRadians)
  def yawSin: Option[Double] = rawRadians.map(Math.sin)
  def yawCos: Option[Double] = rawRadians.map(Math.cos)

  /** Analysis F */
  def caX: Option[Double] = {
    def value = gantryAngleRounded match {
      case 0 => Some(errorX_mm)
      case 180 => Some(-errorX_mm)
      case _ => None
    }
    value.map(rnd)
  }

  /** Analysis G */
  def caY: Option[Double] = {
    def value = gantryAngleRounded match {
      case 90 => Some(errorX_mm)
      case 270 => Some(-errorX_mm)
      case _ => None
    }
    value.map(rnd)
  }

  /** Analysis H */
  def caZ: Option[Double] = Some(-errorY_mm).map(rnd)

  Trace.trace(s"beamName: $beamName    errorX_mm: $errorX_mm   errorY_mm: $errorY_mm   ")
}
