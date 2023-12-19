package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.Config
import org.aqa.Util

import scala.xml.Elem

class SPCenterDose(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Center Dose"

  override val abbreviation: String = "Cntr Dose"

  def toCdBeam(beam: Beam): Elem = ???

  override def selectionList: Seq[Selection] = {
    def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))
    def ok(beam: Beam) = Util.minCenteredFieldBeam(beam.prototypeBeam, Config.CenterDoseRadius_mm * 2)
    val list = beamList.filter(ok).map(toSelection)
    list
  }

  override def setBeamList(beamList: Seq[Beam]): Elem = {
    beamList.map(toCdBeam)
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
