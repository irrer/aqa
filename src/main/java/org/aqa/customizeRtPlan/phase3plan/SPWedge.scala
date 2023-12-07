package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Config

import scala.xml.Elem

class SPWedge(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Wedge"

  private def wedgeSelectionList: Seq[Selection] = {
    def findPair(wedge: Config.WedgeBeam): Option[(Beam, Beam)] = {
      val fg = beamList.find(_.beamName.equals(wedge.wedge))
      val bg = beamList.find(b => wedge.backgroundList.contains(b.beamName))

      if (fg.isDefined && bg.isDefined)
        Some((fg.get, bg.get))
      else
        None
    }

    Config.WedgeBeamList.flatMap(findPair).map(pair => Selection(this, pair._1.beamName, Seq(pair._1, pair._2)))

  }

  override def selectionList: Seq[Selection] = wedgeSelectionList

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
