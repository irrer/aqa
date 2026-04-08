package org.aqa.webrun.stakitt.leafBoundaries

/**
  * Represent a coarsely located peak or valley in a profile. Do this with a list of
  * consecutive indexes that are part of the same peak or valley.
  *
  * @param indexes List of profile indexes.
  * @param isPeak  True if group of indexes is above mean.
  */
case class PeakOrValley(indexes: Seq[Int], isPeak: Boolean, isWide: Boolean = true) {
  val center: Double = indexes.sum / indexes.size.toDouble
  val centerIndex: Int = center.round.toInt
}
