package org.aqa.customizeRtPlan

/**
  * IsoCheck plan without table measurement beams.
  */
class MakeRtplanIsoCheckNoTable extends MakeRtplanIsoCheck {

  override def name: String = "IsoCheck No Table"

  override protected val beamRemovalList: Seq[String] = Seq(
    "G180 C270 T030",
    "G180 C270 T060",
    "G180 C270 T090",
    "G180 C270 T270",
    "G180 C270 T300",
    "G180 C270 T330"
  )
}
