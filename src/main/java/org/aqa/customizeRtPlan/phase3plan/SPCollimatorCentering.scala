package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Config

import scala.xml.Elem

class SPCollimatorCentering(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Collimator Centering"

  override val abbreviation: String = "Col Cntr"

  override def selectionList: Seq[Selection] = {

    val list = beamList.filter(beam => Config.collimatorCenteringPhase3List.contains(beam.beamName))

    if (list.size != 8) throw new RuntimeException(s"Expected to find 8 collimator centering beams in the RTPLAN, but actually found ${list.size}")

    val select1 = {
      val g0 = list.filter(beam => beam.gantryAngle_roundedDeg == 0)
      if (g0.size != 2) throw new RuntimeException(s"Expected to find 2 collimator centering beams in the RTPLAN, but actually found ${list.size}")
      Selection(this, "Gantry 0", g0)
    }

    val select4 = Selection(this, "Gantry 0+90+180+270", list)

    Seq(select1, select4)

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
