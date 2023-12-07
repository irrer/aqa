package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy

/**
  * Encapsulate beam information.
  *
  * @param prototypeBeam To be built from this beam.
  * @param beamName New beam name.
  * @param beamEnergy New beam energy.
  */
case class Beam(prototypeBeam: AttributeList, beamName: String, beamEnergy: MachineBeamEnergy) {

  /** Beam number in metadata. */
  // def beamNumber: Int = al.get(TagByName.BeamNumber).getIntegerValues.head

  /** Beam energy in MeV. */
  // def energy_MeV: Double = DicomUtil.findAllSingle(al, TagByName.NominalBeamEnergy).head.getDoubleValues.head

  /** True if this is an FFF beam. */
  def isFFF: Boolean = beamEnergy.isFFF

  /** First collimator angle in fraction sequence. */
  def colAngle_deg: Double = Util.collimatorAngle(prototypeBeam)

  /** Collimator angle rounded to 90. */
  def colAngle_roundedDeg: Int = Util.angleRoundedTo90(colAngle_deg)

  /** First gantry angle in fraction sequence. */
  def gantryAngle_deg: Double = Util.gantryAngle(prototypeBeam)

  def gantryAngle_roundedDeg: Int = Util.angleRoundedTo90(gantryAngle_deg)

  override def toString: String = {
    val j0 = beamName + ""
    Trace.trace(j0)
    val j1 = beamEnergy.photonEnergy_MeV + ""
    Trace.trace(j1)
    val j2 = beamEnergy.isFFF + ""
    Trace.trace(j2)
    val j3 = colAngle_roundedDeg + ""
    Trace.trace(j3)
    val j4 = gantryAngle_deg + ""
    Trace.trace(j4)
    s"$beamName | ${beamEnergy.photonEnergy_MeV.get} Mev | isFFF: ${beamEnergy.isFFF} | maxDoseRate: ${beamEnergy.maxDoseRate_MUperMin.get} | col angle: $colAngle_roundedDeg  | gantry: $gantryAngle_deg "
  }

}

object Beam {

  /**
    * Given the attribute list of a beam, create a beam.
    * @param machine For this machine.
    * @param beamAl Attribute list of beam.
    * @return Beam reflecting the properties of the attribute list.
    */
  def makeBeamFromAl(machine: Machine, beamAl: AttributeList): Beam = {

    val beamName = beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString()
    val beamEnergy: MachineBeamEnergy = {
      val photonEnergy_MeV = DicomUtil.findAllSingle(beamAl, TagByName.NominalBeamEnergy).head.getDoubleValues.head
      val maxDoseRate_MUperMin = DicomUtil.findAllSingle(beamAl, TagByName.DoseRateSet).head.getDoubleValues.head

      val fffEnergy_MeV: Double = if (CustomizeRtPlanUtil.isFFFBeam(beamAl)) 1.0 else 0
      MachineBeamEnergy(None, machinePK = machine.machinePK.get, Some(photonEnergy_MeV), Some(maxDoseRate_MUperMin), Some(fffEnergy_MeV))
    }
    new Beam(beamAl, beamName, beamEnergy)
  }
}
