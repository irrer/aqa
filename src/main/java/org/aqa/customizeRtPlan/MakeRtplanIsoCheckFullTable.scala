package org.aqa.customizeRtPlan

/**
  * IsoCheck plan with a full set of table measurement beams.
  */
class MakeRtplanIsoCheckFullTable extends MakeRtplanIsoCheck {

  override def name: String = "IsoCheck Full Table"

  override protected val beamRemovalList: Seq[String] = Seq()
}
