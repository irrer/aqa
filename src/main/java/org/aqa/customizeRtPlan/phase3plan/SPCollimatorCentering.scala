package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Config

class SPCollimatorCentering(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Collimator Centering"

  //noinspection SpellCheckingInspection
  override val abbreviation: String = "Col Cntr"

  override def initialSelectionList: Seq[Selection] = {

    val list = beamList.filter(beam => Config.collimatorCenteringPhase3List.contains(beam.beamName))

    if (list.size != 8) throw new RuntimeException(s"Expected to find 8 collimator centering beams in the RTPLAN, but actually found ${list.size}")

    def gantry(angle: Int): Selection = {
      val beamList = list.filter(_.gantryAngle_roundedDeg == angle).sortBy(_.colAngle_roundedDeg)
      Selection(this, s"Gantry $angle", beamList)
    }

    Seq(0, 90, 180, 270).map(gantry)
  }

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
