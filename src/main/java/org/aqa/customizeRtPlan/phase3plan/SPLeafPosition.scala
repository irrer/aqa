package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Config

import scala.xml.Elem

class SPLeafPosition(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Leaf Position / Picket Fence"


  override def selectionList: Seq[Selection] = {
    def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))
    def ok(beam: Beam) = Config.LeafPositionBeamNameList.contains(beam.beamName)
    val list = beamList.filter(ok).map(toSelection)
    list
  }

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
