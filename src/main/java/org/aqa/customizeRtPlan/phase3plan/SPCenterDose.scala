package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Config
import org.aqa.Util

class SPCenterDose(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Center Dose"

  override val abbreviation: String = "Cntr Dose"

  override def selectionList: Seq[Selection] = {
    def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))
    def ok(beam: Beam) = Util.minCenteredFieldBeam(beam.prototypeBeam, Config.CenterDoseRadius_mm * 2)
    val list = beamList.filter(ok).map(toSelection)
    list
  }

  override def getBeamList: Seq[Beam] = selectionList.flatMap(_.beamList)

}
