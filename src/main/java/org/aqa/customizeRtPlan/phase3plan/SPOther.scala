package org.aqa.customizeRtPlan.phase3plan

/**
  * Allow user to select beams that are in the RTPLAN but not assigned to a procedure.  This
  * allows the user to add beams for the user's own purposes.
  * @param metaData Machine metadata
  * @param unclaimedBeamList List of beams that no other test uses.
  */
class SPOther(metaData: SPMetaData, unclaimedBeamList: Seq[Beam]) extends SubProcedure(metaData, unclaimedBeamList: Seq[Beam]) {

  override val name = "Other"

  override val abbreviation: String = "Other"
  private def otherSelectionList: Seq[Selection] = {

    def toSelection(beam: Beam): Selection = {
      Selection(this, beam.beamName, Seq(beam))
    }

    unclaimedBeamList.map(toSelection)

  }

  override def initialSelectionList: Seq[Selection] = otherSelectionList

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
