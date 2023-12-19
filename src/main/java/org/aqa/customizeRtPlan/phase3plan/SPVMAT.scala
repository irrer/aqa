package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Config

import scala.xml.Elem

class SPVMAT(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

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

  override def setBeamList(beamList: Seq[Beam]): Elem = {
    ???
  }

  override def update(checkboxIdList: Seq[String]): Seq[Beam] = {
    ???
  }

  override def getBeamList: Seq[Beam] = selectionList.flatMap(_.beamList)

  override def generatePlan(checkboxIdList: Seq[String]): Seq[AttributeList] = {
    ???
  }

}
