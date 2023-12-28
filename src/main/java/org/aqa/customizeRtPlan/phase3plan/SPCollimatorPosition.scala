package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Util

class SPCollimatorPosition(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Collimator Position"

  override val abbreviation: String = "Coll Posn"

  /** The beam must describe a rectangle at this this tall and wide. */
  private val minSize_mm = 10.0

  private def colPosSelectionList: Seq[Selection] = {
    val list = beamList.filter(beam => Util.minCenteredFieldBeam(beam.prototypeBeam, minSize_mm))
    list.map(beam => Selection(this, beam.beamName, Seq(beam)))
  }

  override def initialSelectionList: Seq[Selection] = colPosSelectionList

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
