package org.aqa.webrun.focalSpot

import org.aqa.Config
import org.aqa.Logging

/**
  * A Complete set of 4 beams that comprise a set of focal spot beams.
  *
  * Yields the focal spot offset values.
  *
  * @param jaw090 Measurements for jaw field with collimator at  90 degrees.
  * @param jaw270 Measurements for jaw field with collimator at 270 degrees.
  * @param mlc090 Measurements for MLC field with collimator at  90 degrees.
  * @param mlc270 Measurements for MLC field with collimator at 270 degrees.
  */
case class FSSet(jaw090: FSMeasure, jaw270: FSMeasure, mlc090: FSMeasure, mlc270: FSMeasure) extends Logging {

  val mvText: String = jaw090.mvText

  val measureList = Seq(jaw090, jaw270, mlc090, mlc270)

  // @formatter:off
  val jawXCenter: Double =
    (
      jaw090.focalSpot.leftEdge_mm  +
      jaw090.focalSpot.rightEdge_mm +
      jaw270.focalSpot.leftEdge_mm  +
      jaw270.focalSpot.rightEdge_mm
    ) / 4

  val mlcXCenter: Double =
    (
      mlc090.focalSpot.leftEdge_mm  +
      mlc090.focalSpot.rightEdge_mm +
      mlc270.focalSpot.leftEdge_mm  +
      mlc270.focalSpot.rightEdge_mm
    ) / 4

  val jawYCenter: Double =
    (
      jaw090.focalSpot.topEdge_mm    +
      jaw090.focalSpot.bottomEdge_mm +
      jaw270.focalSpot.topEdge_mm    +
      jaw270.focalSpot.bottomEdge_mm
      ) / 4

  val mlcYCenter: Double =
    (
      mlc090.focalSpot.topEdge_mm    +
      mlc090.focalSpot.bottomEdge_mm +
      mlc270.focalSpot.topEdge_mm    +
      mlc270.focalSpot.bottomEdge_mm
      ) / 4

  // @formatter:on

  // private val dEpid_mm: Double = measureList.map(_.dEpid_mm).sum / 4.0 // take the average epid value
  private val dIso_mm: Double = measureList.map(_.dIso_mm).sum / 4.0 // take the average ISO value

  logger.info(s"MV ${jaw090.NominalBeamEnergy}    jawXCenter: $jawXCenter    mlcXCenter: $mlcXCenter    jawYCenter: $jawYCenter    mlcYCenter: $mlcYCenter")

  val aX: Double = {
    val xJaw = Config.TrueBeamSourceToXJawDistance_mm
    val col = Config.TrueBeamSourceToMLCDistance_mm
    val jaw = (dIso_mm - xJaw) / xJaw
    val mlc = (dIso_mm - col) / col
    1.0 / (jaw - mlc)
  }

  val aY: Double = {
    val yJaw = Config.TrueBeamSourceToYJawDistance_mm
    val col = Config.TrueBeamSourceToMLCDistance_mm
    val jaw = (dIso_mm - yJaw) / yJaw
    val mlc = (dIso_mm - col) / col
    1.0 / (jaw - mlc)
  }

  logger.info(s"MV ${jaw090.NominalBeamEnergy}    aX: $aX    aY: $aY")

  /** Focal spot alignment for X .  Ideally this should be 0. */
  val focalSpotAlignmentX_mm: Double = aX * (jawXCenter - mlcXCenter)

  /** Focal spot alignment for Y .  Ideally this should be 0. */
  val focalSpotAlignmentY_mm: Double = aY * (jawYCenter - mlcYCenter)

  logger.info(s"MV ${jaw090.NominalBeamEnergy}    focalSpotAlignmentX_mm: $focalSpotAlignmentX_mm    focalSpotAlignmentY_mm: $focalSpotAlignmentY_mm")

  private def showEdges(fm: FSMeasure): String = {
    def fmt(d: Double) = d.formatted("%20.16f")
    (if (fm.isJaw) "Jaw " else "MLC ") +
      fm.collimatorAngleRounded_deg.formatted("%03d") + "   TBLR: " +
      fmt(fm.focalSpot.topEdge_mm) + "    " +
      fmt(fm.focalSpot.bottomEdge_mm) + "    " +
      fmt(fm.focalSpot.leftEdge_mm) + "    " +
      fmt(fm.focalSpot.rightEdge_mm)
  }

  logger.info(s"MV ${jaw090.NominalBeamEnergy} edge positions:\n" + Seq(showEdges(jaw090), showEdges(jaw270), showEdges(mlc090), showEdges(mlc270)).mkString("\n"))

  logger.info(
    s"""
      |MV ${jaw090.NominalBeamEnergy} Centers:
      |  jaw center X: " + ${jawXCenter.formatted("%20.16f")}
      |  jaw center Y: " + ${jawYCenter.formatted("%20.16f")}
      |  MLC center X: " + ${mlcXCenter.formatted("%20.16f")}
      |  MLC center Y: " + ${mlcYCenter.formatted("%20.16f")}
      |""".stripMargin
  )

  val htmlFileName: String = {
    val fffText = if (jaw090.isFFF) "FFF" else "X"
    s"MV${jaw090.mvText}$fffText.html"
  }
}
