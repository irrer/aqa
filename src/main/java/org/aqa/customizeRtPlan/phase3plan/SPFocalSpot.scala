package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.Config
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil
import org.aqa.db.MultileafCollimator
import org.aqa.Util

import scala.xml.Elem

class SPFocalSpot(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator) extends SubProcedure(machine, beamEnergyList, multileafCollimator) {

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

    val prototypeSet = list.filterNot(_.isFFF).groupBy(beam => s"${beam.isFFF} ${beam.energy_MeV}").values.head

    def setEnergyOfSet(machineBeamEnergy: MachineBeamEnergy): Seq[Beam] = {

      def makeBeam(prototypeBeam: AttributeList): Beam = {

        val BeamName = {
          val beamType = if (machineBeamEnergy.isFFF) "fff" else "x"
          val isMLC = DicomUtil.findAllSingle(prototypeBeam, TagByName.LeafJawPositions).map(_.getDoubleValues).exists(ljp => ljp.length > 2)
          val limitName = if (isMLC) "MLC" else "Jaw"
          val colAngle = Util.angleRoundedTo90(Util.collimatorAngle(prototypeBeam))
          s"${machineBeamEnergy.photonEnergy_MeV.get.round.toString}$beamType-10-$limitName-$colAngle"
        }
        val beamAl = CustomizeRtPlanUtil.makeBeam(
          machineEnergy = machineBeamEnergy,
          prototypeBeam,
          BeamName,
          1 // This gets overwritten when the rtplan is created.
        )
        new Beam(beamAl)
      }

      prototypeSet.map(beam => makeBeam(beam.al))
    }

    /**
      * Determine if this beam energy has the necessary data to be used to make beams.
      * @param beamEnergy machine beam energy.
      * @return True if ok
      *         TODO Maybe instead this should assume a default.  Waiting on Justin's input.
      */
    def ok(beamEnergy: MachineBeamEnergy): Boolean = beamEnergy.photonEnergy_MeV.isDefined && beamEnergy.maxDoseRate_MUperMin.isDefined

    val beamList = beamEnergyList.filter(ok).flatMap(energy => setEnergyOfSet(energy))
    beamList
  }

  override def generatePlan(checkboxIdList: Seq[String]): Seq[AttributeList] = {
    ???
  }

}
