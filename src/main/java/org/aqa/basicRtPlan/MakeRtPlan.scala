package org.aqa.basicRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.VarianPrivateTag

/**
  * Given specifications, create a basic custom RTPLAN.
  * @param PatientID Patient ID to use.
  * @param RTPlanLabel Plan label ID to use.
  * @param beamList List of beam specifications.
  */
class MakeRtPlan(
    PatientID: String,
    machineName: String,
    RTPlanLabel: String,
    beamList: Seq[BeamSpecification]
) extends Logging {

  private val templateDir = Config.BasicRtplanTemplateDir.get

  private val rtplanFile = Util.listDirFiles(templateDir).find(f => f.getName.toLowerCase().contains("rtplan")).get
  private val rtplanTemplate = new DicomFile(rtplanFile).attributeList.get

  private val BeamSequence = DicomUtil.seqToAttr(rtplanTemplate, TagByName.BeamSequence)

  /**
    * Given a beam, return its name.
    * @param beamAl Beam sequence item.
    * @return Beam name.
    */
  private def nameOfBeam(beamAl: AttributeList): String = {
    val name = DicomUtil.findAllSingle(beamAl, TagByName.BeamName).head.getSingleStringValueOrEmptyString()
    name
  }

  private def setNominalBeamEnergy(NominalBeamEnergy: Attribute, energy: Double): Unit = {
    NominalBeamEnergy.removeValues()
    NominalBeamEnergy.addValue(energy)
  }

  private def setLeafJawPositions(beamTemplate: AttributeList, deviceTypeList: Seq[String], size_mm: Double): Unit = {
    val positionSeqList = {
      val atSeq = DicomUtil.findAllSingle(beamTemplate, TagByName.BeamLimitingDevicePositionSequence).asInstanceOf[IndexedSeq[SequenceAttribute]]
      val list = atSeq.flatMap(DicomUtil.alOfSeq)
      list
    }

    def typeOk(ps: AttributeList): Boolean = {
      val devType = ps.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString()
      deviceTypeList.contains(devType)
    }

    def setJaw(ps: AttributeList): Unit = {
      val LeafJawPositions = ps.get(TagByName.LeafJawPositions)
      LeafJawPositions.removeValues()
      val half = (size_mm / 2).abs
      LeafJawPositions.addValue(-half)
      LeafJawPositions.addValue(half)
    }

    positionSeqList.filter(ps => typeOk(ps)).foreach(setJaw)
  }

  private def setupBeam(beamSpecification: BeamSpecification): Unit = {

    val beamTemplate = BeamSequence.find(b => nameOfBeam(b).equalsIgnoreCase(beamSpecification.BeamName)).get

    // Find all references to beam energy and set them to the specified level.
    DicomUtil.findAllSingle(beamTemplate, TagByName.NominalBeamEnergy).foreach(nbe => setNominalBeamEnergy(nbe, beamSpecification.NominalBeamEnergy))

    // set the X and Y jaw values
    setLeafJawPositions(beamTemplate, Seq("X", "ASYMX"), beamSpecification.X_mm)
    setLeafJawPositions(beamTemplate, Seq("Y", "ASYMY"), beamSpecification.Y_mm)
  }

  private def setPatientIdAndNameAndMachine(): Unit = {
    val id = rtplanTemplate.get(TagByName.PatientID)
    id.removeValues()
    id.addValue(PatientID)

    val name = rtplanTemplate.get(TagByName.PatientName)
    name.removeValues()
    name.addValue(PatientID)

    val machineList = DicomUtil.findAllSingle(rtplanTemplate, TagByName.TreatmentMachineName)
    machineList.map(machineNameAttr => {
      machineNameAttr.removeValues()
      machineNameAttr.addValue(machineName)
    })
  }

  private def makeNewUIDs(): Unit = {
    // ???  TODO
  }

  private def removeVarianTags(al: AttributeList = rtplanTemplate): Unit = {
    VarianPrivateTag.tagList.map(tag => {
      if (al.get(tag) != null)
        al.remove(tag)
    })
    val saList = al.values().toArray.filter(tag => tag.isInstanceOf[SequenceAttribute]).map(_.asInstanceOf[SequenceAttribute]).toSeq

    saList.flatMap(sa => DicomUtil.alOfSeq(sa)).foreach(removeVarianTags)
  }

  /**
    * Remove beams that are not one of the 4 required.
    */
  private def removeOtherBeams(): Unit = {

    val requiredBeamNumberList = {
      val beamNameList = beamList.map(_.BeamName)
      val allBeams = DicomUtil.seqToAttr(rtplanTemplate, TagByName.BeamSequence)
      val requiredBeams = allBeams.filter(al => beamNameList.contains(nameOfBeam(al)))
      val beamNumberList = requiredBeams.map(al => al.get(TagByName.BeamNumber).getIntegerValues.head)
      beamNumberList
    }

    val refBeamSeq = DicomUtil.seqToAttr(rtplanTemplate, TagByName.FractionGroupSequence).map(al => al.get(TagByName.ReferencedBeamSequence).asInstanceOf[SequenceAttribute])

    refBeamSeq.map(sq => {
      val itemList = (0 until sq.getNumberOfItems).map(i => sq.getItem(i))
      val notNeeded = itemList.filterNot(item => requiredBeamNumberList.contains(item.getAttributeList.get(TagByName.ReferencedBeamNumber).getIntegerValues.head))
      notNeeded.map(nn => sq.remove(nn))
    })

    // TODO remove other beam stuff

  }

  def makeRtplan(): AttributeList = {
    removeVarianTags()
    removeOtherBeams()
    setPatientIdAndNameAndMachine
    makeNewUIDs()
    beamList.foreach(setupBeam)
    rtplanTemplate
  }

}

object MakeRtPlan {
  def main(args: Array[String]): Unit = {
    val g000 = BeamSpecification(GantryAngle_deg = 0, BeamName = Config.BasicRtplanBeamNameG000, X_mm = 25, Y_mm = 26, NominalBeamEnergy = 6)
    val g090 = BeamSpecification(GantryAngle_deg = 90, BeamName = Config.BasicRtplanBeamNameG090, X_mm = 35, Y_mm = 36, NominalBeamEnergy = 6)
    val g180 = BeamSpecification(GantryAngle_deg = 180, BeamName = Config.BasicRtplanBeamNameG180, X_mm = 45, Y_mm = 46, NominalBeamEnergy = 6)
    val g270 = BeamSpecification(GantryAngle_deg = 270, BeamName = Config.BasicRtplanBeamNameG270, X_mm = 55, Y_mm = 56, NominalBeamEnergy = 6)

    val mrp = new MakeRtPlan(PatientID = "Hiya", machineName = "JimMach", RTPlanLabel = "ThePlan", beamList = Seq(g000, g090, g180, g270))
    val plan = mrp.makeRtplan()

    val text = DicomUtil.attributeListToString(plan)
    println("===========================================================================")
    println(text)
    println("===========================================================================")
  }
}
