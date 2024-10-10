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
import edu.umro.ScalaUtil.Trace
import org.aqa.db.MachineBeamEnergy
import org.aqa.Config
import org.aqa.Util

object SPFocalSpot {
  val name = "Focal Spot"

  /**
    * True if the field is defined by the MLC.
    * @param prototypeBeam Attribute list in the RTPLAN for the beam of interest.
    * @return True if MLC, false if jaw.
    */
  def isMLC(prototypeBeam: AttributeList): Boolean = {
    DicomUtil.findAllSingle(prototypeBeam, TagByName.LeafJawPositions).map(_.getDoubleValues).exists(ljp => ljp.length > 2)
  }

  /**
    * True if the field is defined by the jaw.
    * @param prototypeBeam Attribute list in the RTPLAN for the beam of interest.
    * @return True if jaw, false if MLC.
    */
  def isJaw(prototypeBeam: AttributeList): Boolean = !isMLC(prototypeBeam)

  /**
    * Make a name for a beam.
    * @param machineBeamEnergy For this energy.
    * @param prototypeBeam Based on this beam.
    * @return Name of beam.
    */
  private def makeBeamName(machineBeamEnergy: MachineBeamEnergy, prototypeBeam: AttributeList) = {
    val beamType = if (machineBeamEnergy.isFFF) "fff" else "x"
    val limitName = if (isMLC(prototypeBeam)) "MLC" else "Jaw"
    val colAngle = Util.angleRoundedTo90(Util.collimatorAngle(prototypeBeam))
    s"${machineBeamEnergy.photonEnergy_MeV.get.round.toString}$beamType-10-$limitName-$colAngle"
  }

  def setEnergyOfSet(prototypeSet: Seq[Beam], machineBeamEnergy: MachineBeamEnergy): Seq[Beam] = {

    def makeBeam(prototypeBeam: AttributeList): Beam = {
      val beamName = makeBeamName(machineBeamEnergy, prototypeBeam)
      if (beamName.contains("loo"))
        Trace.trace("hey")
      new Beam(prototypeBeam, beamName, machineBeamEnergy)
    }

    val beamList = prototypeSet.map(beam => makeBeam(beam.prototypeBeam))
    beamList
  }

}

class SPFocalSpot(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name: String = SPFocalSpot.name

  //noinspection SpellCheckingInspection
  override val abbreviation: String = "Focl Spot"

  private val fsPrototypeList =
    beamList.filter(beam => Config.FocalSpotBeamNameList.contains(beam.beamName)).sortBy(_.beamName)

  private val prototypeSet = fsPrototypeList.filterNot(_.isFFF).groupBy(beam => s"${beam.isFFF} ${beam.beamEnergy.photonEnergy_MeV.get}").values.head

  private val sfSelectionList = metaData.beamEnergyList.map(energy => SPFocalSpot.setEnergyOfSet(prototypeSet, energy))

  override val usesCollimatorCentering: Boolean = false

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

  private def beamSetToSelection(beamSet: Seq[Beam]): Selection = {
    val machineBeamEnergy = beamSet.head.beamEnergy
    val energyType = if (machineBeamEnergy.isFFF) "FFF" else "X"
    val energyText = {
      val e = machineBeamEnergy.photonEnergy_MeV.get
      if (e.round == e)
        e.round.toString
      else
        Util.fmtDbl(e)
    }
    val selectionName = s"$energyText $energyType"
    Selection(this, selectionName, beamSet)
  }

  override def initialSelectionList: Seq[Selection] = {
    val list = sfSelectionList.map(beamSetToSelection)
    list
  }

  override def consecutivelyDeliveredBeamSets: Seq[Seq[Beam]] = {

    /** Split into MLC and jaw pairs. */
    def selToSets(selection: Selection): Seq[Seq[Beam]] = {
      selection.beamList.groupBy(beam => SPFocalSpot.isMLC(beam.prototypeBeam)).values.map(_.toSeq).toSeq
    }

    selectionList.flatMap(selToSets)
  }

}
