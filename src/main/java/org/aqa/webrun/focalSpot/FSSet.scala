package org.aqa.webrun.focalSpot

import org.aqa.Config

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
case class FSSet(jaw090: FSMeasure, jaw270: FSMeasure, mlc090: FSMeasure, mlc270: FSMeasure) {

  private val measureList = Seq(jaw090, jaw270, mlc090, mlc270)

  // @formatter:off
  private val jawXCenter: Double =
    (
      jaw090.focalSpot.topEdge_mm    +
      jaw090.focalSpot.bottomEdge_mm +
      jaw270.focalSpot.topEdge_mm    +
      jaw270.focalSpot.bottomEdge_mm
    ) / 4

  private val mlcXCenter: Double =
    (
      mlc090.focalSpot.topEdge_mm    +
      mlc090.focalSpot.bottomEdge_mm +
      mlc270.focalSpot.topEdge_mm    +
      mlc270.focalSpot.bottomEdge_mm
    ) / 4

  private val jawYCenter: Double =
    (
      jaw090.focalSpot.leftEdge_mm  +
      jaw090.focalSpot.rightEdge_mm +
      jaw270.focalSpot.leftEdge_mm  +
      jaw270.focalSpot.rightEdge_mm
    ) / 4

  private val mlcYCenter: Double =
    (
      mlc090.focalSpot.leftEdge_mm  +
      mlc090.focalSpot.rightEdge_mm +
      mlc270.focalSpot.leftEdge_mm  +
      mlc270.focalSpot.rightEdge_mm
    ) / 4
  // @formatter:on

  private val dEpid_mm: Double = measureList.map(_.dEpid_mm).sum / 4.0 // take the average epid value

  private val aX = {
    val xJaw = Config.TrueBeamSourceToXJawDistance_mm
    val col = Config.TrueBeamSourceToMLCDistance_mm
    val jaw = (dEpid_mm - xJaw) / xJaw
    val mlc = (dEpid_mm - col) / col
    1.0 / (jaw - mlc)
  }

  private val aY = {
    val yJaw = Config.TrueBeamSourceToYJawDistance_mm
    val col = Config.TrueBeamSourceToMLCDistance_mm
    val jaw = (dEpid_mm - yJaw) / yJaw
    val mlc = (dEpid_mm - col) / col
    1.0 / (jaw - mlc)
  }

  /** Focal spot alignment for X .  Ideally this should be 0. */
  val focalSpotAlignmentX_mm: Double = aX * (jawXCenter - mlcXCenter)

  /** Focal spot alignment for Y .  Ideally this should be 0. */
  val focalSpotAlignmentY_mm: Double = aY * (jawYCenter - mlcYCenter)

  val htmlFileName: String = {
    val text = {
      if (jaw090.NominalBeamEnergy.round == jaw090.NominalBeamEnergy)
        jaw090.NominalBeamEnergy.round.toString
      else
        jaw090.NominalBeamEnergy.toString
    }
    s"MV$text.html"
  }
}
