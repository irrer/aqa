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

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.MachineBeamEnergy

class SPSymFlatConst(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Symmetry Flatness"

  override val abbreviation: String = "Sym Flat"

  /**
    * Make a list of machine beam energies that are not covered (defined) in the prototype plan.
    * @param coveredBeamList SymFlat beams that are defined in the prototype plan.
    * @return List of energies that this machine supports but are not represented in the coveredBeamList.
    */
  private def makeUnusedEnergyList(coveredBeamList: Seq[Selection]) = {

    def energySig(e: MachineBeamEnergy): String = {
      Seq(e.photonEnergy_MeV, e.maxDoseRate_MUperMin, e.fffEnergy_MeV).flatten.map(_.toString).mkString(" | ")
    }

    def energySignature(beam: Beam): String = energySig(beam.beamEnergy)

    // list of
    val coveredEnergyList = coveredBeamList.flatMap(_.beamList).groupBy(energySignature).values.map(_.head.beamEnergy)

    def inKnownList(e: MachineBeamEnergy): Boolean = {
      val eSig = energySig(e)
      coveredEnergyList.exists(cov => energySig(cov).equals(eSig))
    }

    val missingEnergyList = metaData.beamEnergyList.filterNot(inKnownList)

    missingEnergyList
  }

  private def toBeam(beamEnergy: MachineBeamEnergy, prototypeBeam: Beam): Beam = {
    val beamName = {
      val prefix = prototypeBeam.beamName.take(4) // example beam names: J20G0-6F, J18G0-6X
      val gantryAngle = prototypeBeam.gantryAngle_roundedDeg
      val kvp = {
        val e = beamEnergy.photonEnergy_MeV.get
        if (e.round == e)
          e.round.toString
        else
          Util.fmtDbl(beamEnergy.photonEnergy_MeV.get)
      }
      val fff = if (beamEnergy.isFFF) "F" else "X"
      s"$prefix$gantryAngle-$kvp$fff"
    }

    Beam(prototypeBeam.prototypeBeam, beamName, beamEnergy)
  }

  private def toSelection(beam: Beam): Selection = Selection(this, beam.beamName, Seq(beam))

  private def makeNewBeamList(coveredBeamList: Seq[Selection]): Seq[Selection] = {
    val unusedEnergyList = makeUnusedEnergyList(coveredBeamList)

    def isFlood(selection: Selection): Boolean = selection.beamList.head.beamName.toLowerCase().contains("flood")

    val prototypeBeam = coveredBeamList.filterNot(isFlood).minBy(_.beamList.head.gantryAngle_roundedDeg).beamList.head

    val beamList = unusedEnergyList.map(e => toBeam(e, prototypeBeam))
    beamList.map(toSelection)
  }

  override def initialSelectionList: Seq[Selection] = {
    val margin = Config.SymmetryAndFlatnessDiameter_mm / 2
    val max = {
      val m = Config.SymmetryAndFlatnessPointList.flatMap(p => Seq(p.x_mm, p.y_mm)).flatMap(c => Seq(c + margin, c - margin))
      m.max - m.min
    }

    def ok(beam: Beam) = Util.minCenteredFieldBeam(beam.prototypeBeam, max)
    val list = beamList.filter(ok).map(toSelection)

    val newBeamList = makeNewBeamList(list)

    list ++ newBeamList
  }

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

}
