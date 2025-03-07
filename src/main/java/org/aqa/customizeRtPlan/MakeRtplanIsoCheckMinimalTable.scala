package org.aqa.customizeRtPlan

/**
  * IsoCheck plan with minimal table measurement beams.
  */
class MakeRtplanIsoCheckMinimalTable extends MakeRtplanIsoCheck {

  override def name: String = "IsoCheck Minimal Table"

  override protected val beamRemovalList: Seq[String] = Seq(
    "G180 C270 T030",
    "G180 C270 T060",
    "G180 C270 T300",
    "G180 C270 T330"
  )
}
