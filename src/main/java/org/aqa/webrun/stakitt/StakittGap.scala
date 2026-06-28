package org.aqa.webrun.stakitt

import org.aqa.Logging

/**
  * A pair of opposing leaf ends that define a gap.
  *
  * @param x1 Leaf from X1 bank of collimator.
  * @param x2 Leaf from X1 bank of collimator.
  */
case class StakittGap(x1: StakittResult, x2: StakittResult) extends Logging {

  /** Difference of x2 - x1 planned  */
  val error: Double = (x2.stakitt.measuredEndPosition_mm - x1.stakitt.measuredEndPosition_mm) - (x2.stakitt.plannedEndPosition_mm - x1.stakitt.plannedEndPosition_mm)
}
