package org.aqa.customizeRtPlan.phase3plan

import org.aqa.Config

class SPVmat(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "VMAT"

  override val abbreviation: String = "VMAT"
  private def vmatSelectionList: Seq[Selection] = {
    def findPair(vmat: Config.VMATBeamPair): Option[(Beam, Beam)] = {
      val mlc = beamList.find(_.beamName.equals(vmat.MLC))
      val open = beamList.find(b => vmat.OPEN.contains(b.beamName))

      if (mlc.isDefined && open.isDefined)
        Some((mlc.get, open.get))
      else
        None
    }

    Config.VMATBeamPairList.flatMap(findPair).map(pair => Selection(this, pair._1.beamName, Seq(pair._1, pair._2)))

  }

  override def selectionList: Seq[Selection] = vmatSelectionList

  override def getBeamList: Seq[Beam] = selectionList.flatMap(_.beamList)

}
