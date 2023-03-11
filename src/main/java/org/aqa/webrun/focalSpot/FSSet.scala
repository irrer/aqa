package org.aqa.webrun.focalSpot

/**
  * A Complete set of 4 beams that comprise a set of focal spot beams.
  *
  * @param jaw090 Measurements for jaw field with collimator at  90 degrees.
  * @param jaw270 Measurements for jaw field with collimator at 270 degrees.
  * @param mlc090 Measurements for MLC field with collimator at  90 degrees.
  * @param mlc270 Measurements for MLC field with collimator at 270 degrees.
  */
case class FSSet(jaw090: FSMeasure, jaw270: FSMeasure, mlc090: FSMeasure, mlc270: FSMeasure) {
  // @formatter:off
  val jawCenter =
    (
      jaw090.focalSpot.leftEdge_mm  +
      jaw090.focalSpot.rightEdge_mm +
      jaw270.focalSpot.leftEdge_mm  +
      jaw270.focalSpot.rightEdge_mm
    ) / 4

  val mlcCenter =
    (
      mlc090.focalSpot.leftEdge_mm  +
      mlc090.focalSpot.rightEdge_mm +
      mlc270.focalSpot.leftEdge_mm  +
      mlc270.focalSpot.rightEdge_mm
    ) / 4
  // @formatter:on

  /** Focal spot alignment.  Ideally this should be 0. */
  val alignment = jawCenter - mlcCenter

  val matlab =
    s"""
       |
       |""".stripMargin
}
