package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.GapSkew
import org.aqa.webrun.gapSkew.GapOffsetSkew

import scala.Double.NaN

object GapSkewCsvX2 {

  private def prototype =
    GapSkew(
      gapSkewPK = Some(-1),
      outputPK = -1.toLong,
      rtimageUID = "",
      beamName = "",
      collimatorAngle_deg = NaN,
      //
      measurementWidth_mm = NaN,
      measurementSeparation_mm = NaN,
      //
      topLeftEdgeTypeName = Some("filler"),
      topLeftValue_mm = Some(NaN),
      topLeftPlanned_mm = Some(NaN),
      //
      topRightEdgeTypeName = Some("filler"),
      topRightValue_mm = Some(NaN),
      topRightPlanned_mm = Some(NaN),
      //
      bottomLeftEdgeTypeName = Some("filler"),
      bottomLeftValue_mm = Some(NaN),
      bottomLeftPlanned_mm = Some(NaN),
      //
      bottomRightEdgeTypeName = Some("filler"),
      bottomRightValue_mm = Some(NaN),
      bottomRightPlanned_mm = Some(NaN)
    )

  // @formatter:off

  private def c090A = prototype.copy(
    collimatorAngle_deg = 90,
    topLeftEdgeTypeName = Some("X2 MLC Horz"),
    topRightEdgeTypeName = Some("X2 MLC Horz"),
    bottomLeftEdgeTypeName = Some("X1 Jaw Horz"),
    bottomRightEdgeTypeName = Some("X1 Jaw Horz"))

  private def c090B = prototype.copy(
    collimatorAngle_deg = 90,
    topLeftEdgeTypeName = Some("X2 Jaw Horz"),
    topRightEdgeTypeName = Some("X2 Jaw Horz"),
    bottomLeftEdgeTypeName = Some("X1 MLC Horz"),
    bottomRightEdgeTypeName = Some("X1 MLC Horz"))

  private def c270A = prototype.copy(
    collimatorAngle_deg = 270,
    topLeftEdgeTypeName = Some("X1 Jaw Horz"),
    topRightEdgeTypeName = Some("X1 Jaw Horz"),
    bottomLeftEdgeTypeName = Some("X2 MLC Horz"),
    bottomRightEdgeTypeName = Some("X2 MLC Horz"))

  private def c270B = prototype.copy(
    collimatorAngle_deg = 270,
    topLeftEdgeTypeName = Some("X1 MLC Horz"),
    topRightEdgeTypeName = Some("X1 MLC Horz"),
    bottomLeftEdgeTypeName = Some("X2 Jaw Horz"),
    bottomRightEdgeTypeName = Some("X2 Jaw Horz"))

  private def c270Jaw = prototype.copy(
    collimatorAngle_deg = 270,
    topLeftEdgeTypeName = Some("X1 Jaw Horz"),
    topRightEdgeTypeName = Some("X1 Jaw Horz"),
    bottomLeftEdgeTypeName = Some("X2 Jaw Horz"),
    bottomRightEdgeTypeName = Some("X2 Jaw Horz"))
  // @formatter:on

  def staticGapOffsetSkew = new GapOffsetSkew(c090A, c090B, c270A, c270B, c270Jaw)
}
