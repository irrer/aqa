package org.aqa.db

trait WinstonLutzGeneric {
  // @formatter:off
  val PK                       : Option[Long]
  val outputPK                 : Long
  val rtimageUID               : String
  val beamName                 : Option[String]
  val gantryAngle_deg          : Double
  val collimatorAngle_deg      : Double
  val tableAngle_deg           : Option[Double]
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
}
