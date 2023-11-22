package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.Config
import org.aqa.db.MultileafCollimator
import org.aqa.Util

import java.io.File
import scala.xml.Elem

class SPCenterDose(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator, exampleImageFileList: Seq[File],  prototypeBeamList: Seq[Beam])
    extends SubProcedure(machine, beamEnergyList, multileafCollimator, exampleImageFileList, prototypeBeamList) {

  override val name = "Center Dose"

  override val abbreviation = "CD"

  def toCdBeam(beam: Beam): Elem = ???

  override def selectionList: Seq[Selection] = {
    def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))
    def ok(beam: Beam) = Util.minCenteredFieldBeam(beam.prototypeBeam, Config.CenterDoseRadius_mm * 2)
    val list = prototypeBeamList.filter(ok).map(toSelection)
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
