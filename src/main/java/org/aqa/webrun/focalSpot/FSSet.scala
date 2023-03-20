package org.aqa.webrun.focalSpot

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
  val jawXCenter: Double =
    (
      jaw090.focalSpot.topEdge_mm    +
      jaw090.focalSpot.bottomEdge_mm +
      jaw270.focalSpot.topEdge_mm    +
      jaw270.focalSpot.bottomEdge_mm
    ) / 4

  val mlcXCenter: Double =
    (
      mlc090.focalSpot.topEdge_mm    +
      mlc090.focalSpot.bottomEdge_mm +
      mlc270.focalSpot.topEdge_mm    +
      mlc270.focalSpot.bottomEdge_mm
    ) / 4

  val jawYCenter: Double =
    (
      jaw090.focalSpot.leftEdge_mm  +
      jaw090.focalSpot.rightEdge_mm +
      jaw270.focalSpot.leftEdge_mm  +
      jaw270.focalSpot.rightEdge_mm
    ) / 4

  val mlcYCenter: Double =
    (
      mlc090.focalSpot.leftEdge_mm  +
      mlc090.focalSpot.rightEdge_mm +
      mlc270.focalSpot.leftEdge_mm  +
      mlc270.focalSpot.rightEdge_mm
    ) / 4
  // @formatter:on

  val dEpid_mm: Double = measureList.map(_.dEpid_mm).sum / 4.0 // take the average epid value

  private val aX = {
    import org.aqa.webrun.focalSpot.FSSet._
    val jaw = (dEpid_mm - dXJaw_mm) / dXJaw_mm
    val mlc = (dEpid_mm - dXMlc_mm) / dXMlc_mm
    1.0 / (jaw - mlc)
  }

  private val aY = {
    import org.aqa.webrun.focalSpot.FSSet._
    val jaw = (dEpid_mm - dYJaw_mm) / dYJaw_mm
    val mlc = (dEpid_mm - dXMlc_mm) / dXMlc_mm
    1.0 / (jaw - mlc)
  }

  /** Focal spot alignment for X .  Ideally this should be 0. */
  val focalSpotAlignmentX_mm: Double = aX * (jawXCenter - mlcXCenter)

  /** Focal spot alignment for Y .  Ideally this should be 0. */
  val focalSpotAlignmentY_mm: Double = aY * (jawYCenter - mlcYCenter)

  val matlab: String =
    s"""
       |
       |""".stripMargin

  val htmlFileName = {
    val text = {
      if (jaw090.NominalBeamEnergy.round == jaw090.NominalBeamEnergy)
        jaw090.NominalBeamEnergy.round.toString
      else
        jaw090.NominalBeamEnergy.toString
    }
    s"MV${text}.html"
  }
}

object FSSet {
  // TODO establish X and Y source to jaw and source to MLC distances.  This should be done in the collimator configuration in the database.

  val dXJaw_mm: Double = 406.0
  val dYJaw_mm: Double = 319.0
  val dXMlc_mm: Double = 490.0
}
