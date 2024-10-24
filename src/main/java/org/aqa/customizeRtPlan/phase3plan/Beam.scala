package org.aqa.customizeRtPlan.phase3plan

/*
 * Copyright 2024 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
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

  /** List all gantry angles visited by this beam in the order they were visited. */
  def gantryAngleList_deg: Seq[Double] = DicomUtil.findAllSingle(prototypeBeam, TagByName.GantryAngle).flatMap(_.getDoubleValues).distinct.sorted

  def gantryAngle_roundedDeg: Int = Util.angleRoundedTo90(gantryAngle_deg)

  override def toString: String = {
    val mdr = if (beamEnergy.maxDoseRate_MUperMin.isDefined) s""" maxDoseRate: ${"%6.1f".format(beamEnergy.maxDoseRate_MUperMin.get)} |""" else "  NA  "
    //noinspection SpellCheckingInspection
    s"""${"%-24s".format(beamName)} |""" +
      s""" ${"%4.1f".format(beamEnergy.photonEnergy_MeV.get)} Mev |""" +
      s""" isFFF: ${"%5s".format(beamEnergy.isFFF)} |""" +
      s""" maxDoseRate: $mdr |""" +
      s""" col angle: ${"%3d".format(colAngle_roundedDeg)}  |""" +
      s""" gantry angle list: ${gantryAngleList_deg.map(ga => Util.fmtDbl(ga)).mkString("  ")} """
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
      //noinspection SpellCheckingInspection
      val maxDoseRate_MUperMin = DicomUtil.findAllSingle(beamAl, TagByName.DoseRateSet).head.getDoubleValues.head

      val fffEnergy_MeV: Double = if (CustomizeRtPlanUtil.isFFFBeam(beamAl)) 1.0 else 0
      MachineBeamEnergy(None, machinePK = machine.machinePK.get, Some(photonEnergy_MeV), Some(maxDoseRate_MUperMin), Some(fffEnergy_MeV))
    }
    new Beam(beamAl, beamName, beamEnergy)
  }

  /**
    * Make a list of beams from the given plan.
    * @param rtplan  For this plan.
    * @param machine For this machine.
    * @return
    */
  def beamsFromPlan(rtplan: AttributeList, machine: Machine): Seq[Beam] = {
    DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence).map(al => Beam.makeBeamFromAl(machine, al))
  }

}
