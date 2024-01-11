package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.MachineBeamEnergy
import org.aqa.Config
import org.aqa.Util

object SPFocalSpot {
  val name = "Focal Spot"
}

class SPFocalSpot(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name: String = SPFocalSpot.name

  //noinspection SpellCheckingInspection
  override val abbreviation: String = "Focl Spot"

  private val fsPrototypeList =
    beamList.filter(beam => Config.FocalSpotBeamNameList.contains(beam.beamName)).sortBy(_.beamName)

  private val prototypeSet = fsPrototypeList.filterNot(_.isFFF).groupBy(beam => s"${beam.isFFF} ${beam.beamEnergy.photonEnergy_MeV.get}").values.head

  /**
    * True if the field is defined by the MLC.
    * @param prototypeBeam Attribute list in the RTPLAN for the beam of interest.
    * @return True if MLC, false if jaw.
    */
  private def isMLC(prototypeBeam: AttributeList): Boolean = {
    DicomUtil.findAllSingle(prototypeBeam, TagByName.LeafJawPositions).map(_.getDoubleValues).exists(ljp => ljp.length > 2)
  }

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

  private def setEnergyOfSet(prototypeSet: Seq[Beam], machineBeamEnergy: MachineBeamEnergy): Selection = {

    def makeBeam(prototypeBeam: AttributeList): Beam = {

      val beamName = makeBeamName(machineBeamEnergy, prototypeBeam)

      new Beam(prototypeBeam, beamName, machineBeamEnergy)
    }

    val beamList = prototypeSet.map(beam => makeBeam(beam.prototypeBeam))
    val energyType = if (machineBeamEnergy.isFFF) "FFF" else "X"
    val energyText = {
      val e = machineBeamEnergy.photonEnergy_MeV.get
      if (e.round == e)
        e.round.toString
      else
        Util.fmtDbl(e)
    }
    val selectionName = s"$energyText $energyType"
    Selection(this, selectionName, beamList)
  }

  private val sfSelectionList = metaData.beamEnergyList.map(energy => setEnergyOfSet(prototypeSet, energy))

  override def initialSelectionList: Seq[Selection] = sfSelectionList

  override val usesCollimatorCentering: Boolean = true // false // Not sure if this is right.

  override def getBeamList: Seq[Beam] = initialSelectionList.flatMap(_.beamList)

  override def consecutivelyDeliveredBeamSets: Seq[Seq[Beam]] = {

    /** Split into MLC and jaw pairs. */
    def selToSets(selection: Selection): Seq[Seq[Beam]] = {
      selection.beamList.groupBy(beam => isMLC(beam.prototypeBeam)).values.map(_.toSeq).toSeq
    }

    selectionList.flatMap(selToSets)
  }

}
