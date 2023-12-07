package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.MachineBeamEnergy
import org.aqa.Config
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil
import org.aqa.Util

import scala.xml.Elem

class SPFocalSpot(metaData: SPMetaData, beamList: Seq[Beam]) extends SubProcedure(metaData, beamList: Seq[Beam]) {

  override val name = "Focal Spot"

  private val fsPrototypeList =
    beamList.filter(beam => Config.FocalSpotBeamNameList.contains(beam.beamName)).sortBy(_.beamName)

  private val prototypeSet = fsPrototypeList.filterNot(_.isFFF).groupBy(beam => s"${beam.isFFF} ${beam.beamEnergy.photonEnergy_MeV.get}").values.head

  /**
    * Make a name for a beam.
    * @param machineBeamEnergy For this energy.
    * @param prototypeBeam Based on this beam.
    * @return Name of beam.
    */
  private def makeBeamName(machineBeamEnergy: MachineBeamEnergy, prototypeBeam: AttributeList) = {
    val beamType = if (machineBeamEnergy.isFFF) "fff" else "x"
    val isMLC = DicomUtil.findAllSingle(prototypeBeam, TagByName.LeafJawPositions).map(_.getDoubleValues).exists(ljp => ljp.length > 2)
    val limitName = if (isMLC) "MLC" else "Jaw"
    val colAngle = Util.angleRoundedTo90(Util.collimatorAngle(prototypeBeam))
    s"${machineBeamEnergy.photonEnergy_MeV.get.round.toString}$beamType-10-$limitName-$colAngle"
  }

  private def setEnergyOfSet(prototypeSet: Seq[Beam], machineBeamEnergy: MachineBeamEnergy): Selection = {

    def makeBeam(prototypeBeam: AttributeList): Beam = {

      val beamName = makeBeamName(machineBeamEnergy, prototypeBeam)

      val beamAl = CustomizeRtPlanUtil.makeBeam(
        machineEnergy = machineBeamEnergy,
        prototypeBeam,
        beamName,
        1 // This gets overwritten when the rtplan is created.
      )
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

  def toFsBeam(beam: Beam): Elem = ???

  private def beamNameToId(beamName: String): String = {
    val pair = beamName.split("-").last
    pair
  }

  override def selectionList: Seq[Selection] = sfSelectionList

  override def setBeamList(beamList: Seq[Beam]): Elem = {
    beamList.map(toFsBeam)
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
