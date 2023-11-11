package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.Config

import scala.xml.Elem

class SPFocalSpot(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy]) extends SubProcedure(machine, beamEnergyList) {

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

  def toFsBeam(beam: Beam): Elem = ???

  private def beamNameToId(beamName: String): String = {
    val pair = beamName.split("-").last
    pair
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
  override def getBeamList(prototypeBeamList: Seq[Beam]): Seq[Beam] = {
    val list = prototypeBeamList.filter(beam => Config.FocalSpotBeamNameList.contains(beam.beamName)).sortBy(_.beamName)

    Trace.trace(list.mkString("\n")) // TODO rm
    val fffPrototype = list.filter(_.isFFF).groupBy(_.energy_MeV).values.head
    val normalProtoType = list.filterNot(_.isFFF).groupBy(_.energy_MeV).values.head

    def setEnergy(beam: Beam, energy: MachineBeamEnergy, beamType: String): Beam = {
      val isMLC = DicomUtil.findAllSingle(beam.al, TagByName.LeafJawPositions).map(_.getDoubleValues).exists(ljp => ljp.length > 2)
      val limitName = if (isMLC) "MLC" else "JAW"

      val beamName = s"${energy.fffEnergy_MeV.get.round.toString}$beamType-10-$limitName-${beam.colAngle_roundedDeg}"

      val newAl = DicomUtil.clone(beam.al)

      def setBeamName: Unit = {
        val at = newAl.get(TagByName.BeamName)
        at.removeValues()
        at.addValue(beamName)
      }

      def setNominalBeamEnergy: Unit = {
        val at = newAl.get(TagByName.NominalBeamEnergy)
        at.removeValues()
        at.addValue(energy.fffEnergy_MeV.get)
      }

      setBeamName
      setNominalBeamEnergy

      Beam(newAl)
    }

    def setEnergyOfSet(energy: MachineBeamEnergy, beamList: Seq[Beam], beamType: String): Seq[Beam] = {
      beamList.map(beam => setEnergy(beam, energy, beamType))
    }

    val fff = beamEnergyList.filter(_.isFFF).map(energy => setEnergyOfSet(energy, fffPrototype, "fff"))
    val normal = beamEnergyList.filterNot(_.isFFF).map(energy => setEnergyOfSet(energy, normalProtoType, "X"))
    val both = (fff ++ normal).flatten
    both
  }

  override def generatePlan(checkboxIdList: Seq[String]): Seq[AttributeList] = {
    ???
  }

}
