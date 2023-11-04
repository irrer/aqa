package org.aqa.phase3plan
import com.pixelmed.dicom.AttributeList
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy

import scala.xml.Elem

class SPFocalSpot(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], allBeamsList: Seq[Beam]) extends SubProcedure(machine, beamEnergyList, allBeamsList) {

  override val name = "FocalSpot"

  private case class FSBeam(energy: Double, fff: Boolean, isJaw: Boolean, colAngle: Int) {}

  /**
    * Construct a unique checkbox ID from a beam energy.  They are constructed using the beam energy and the FFF flag.
    * @param machineBeamEnergy For this energy.
    * @return HTML ID
    */
  private def toCheckboxId(machineBeamEnergy: MachineBeamEnergy): String = {
    def energy = machineBeamEnergy.photonEnergy_MeV.get.toString.replaceAll("0$", "").replaceAll("\\.$", "")
    def kind = if (machineBeamEnergy.fffEnergy_MeV.isDefined && (machineBeamEnergy.fffEnergy_MeV.get != 0)) "X" else "FFF"
    s"$name-$energy-$kind"
  }

  private def toFsBeam(beam: Beam): Elem

  private def beamNameToId(beamName: String): String = {
    val pair = beamName.split("-").tail

  }

  override def checkboxIdList: Seq[String] = {
    beamEnergyList.map(toCheckboxId)
  }

  override def setBeamList(beamList: Seq[Beam]): Elem = {
    beamList.map(toFsBeam)
    ???
  }

  override def update(checkboxIdList: Seq[String]): Seq[Beam] = {
    ???
  }

  override def generatePlan(checkboxIdList: Seq[String]): Seq[AttributeList] = {
    ???
  }

}
