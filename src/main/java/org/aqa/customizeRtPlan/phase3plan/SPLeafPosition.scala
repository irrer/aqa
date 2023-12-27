package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Config

class SPLeafPosition(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Leaf Position / Picket Fence"

  override val abbreviation: String = "Leaf Pos"

  override def selectionList: Seq[Selection] = {
    def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))
    def ok(beam: Beam) = Config.LeafPositionBeamNameList.contains(beam.beamName)
    val list = beamList.filter(ok).map(toSelection)
    list
  }

  override def getBeamList: Seq[Beam] = selectionList.flatMap(_.beamList)

}
