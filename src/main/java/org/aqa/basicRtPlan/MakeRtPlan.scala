package org.aqa.basicRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.SequenceItem
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.UMROGUID
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util

import java.util.Date

/**
  * Given specifications, create a basic custom RTPLAN.
  * @param PatientID Patient ID to use.
  * @param RTPlanLabel Plan label ID to use.
  * @param beamList List of beam specifications.
  */
class MakeRtPlan(
    PatientID: String,
    PatientName: String,
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

  /**
    * Set all attributes with the given tag to the given value.
    * @param tag Tag to find.
    * @param value New value.
    */
  private def setAll(tag: AttributeTag, value: String): Unit = {
    val list = DicomUtil.findAllSingle(rtplanTemplate, tag)

    list.foreach(attr => {
      attr.removeValues()
      attr.addValue(value)
    })
  }

  private def setVariousAttributes(): Unit = {
    setAll(TagByName.PatientID, PatientID)

    setAll(TagByName.PatientName, PatientName)

    setAll(TagByName.TreatmentMachineName, machineName)
    setAll(TagByName.PatientSex, "O")
    setAll(TagByName.RTPlanLabel, RTPlanLabel)

    setAll(TagByName.Manufacturer, "AQA")
    setAll(TagByName.ManufacturerModelName, "Basic Plan")
    setAll(TagByName.DeviceSerialNumber, "001")
    setAll(TagByName.SoftwareVersions, "0.0.1")
  }

  private def setDatesAndTimes(): Unit = {
    val now = new Date()
    val dateText = DicomUtil.dicomDateFormat.format(now)
    val timeText = DicomUtil.dicomTimeFormat.format(now)

    def setDates(attr: Attribute): Boolean = {
      if (ValueRepresentation.isDateVR(attr.getVR)) {
        attr.removeValues()
        attr.addValue(dateText)
      }
      false
    }

    def setTimes(attr: Attribute): Boolean = {
      if (ValueRepresentation.isTimeVR(attr.getVR)) {
        attr.removeValues()
        attr.addValue(timeText)
      }
      false
    }

    DicomUtil.findAll(rtplanTemplate, setDates _)
    DicomUtil.findAll(rtplanTemplate, setTimes _)

    setAll(TagByName.PatientBirthDate, "18000101")
    setAll(TagByName.PatientBirthTime, "000000")
  }

  private def makeNewUIDs(): Unit = {
    val uidSet = Config.ToBeAnonymizedList.keySet.filter(tag => ValueRepresentation.isUniqueIdentifierVR(DicomUtil.dictionary.getValueRepresentationFromTag(tag)))
    val attrList = DicomUtil.findAll(rtplanTemplate, uidSet)
    val replaceMap = attrList.map(at => at.getSingleStringValueOrEmptyString).distinct.map(uid => (uid, UMROGUID.getUID)).toMap
    def replace(at: Attribute): Unit = {
      val uid = replaceMap(at.getSingleStringValueOrEmptyString)
      at.removeValues()
      at.addValue(uid)
    }
    attrList.foreach(at => replace(at))
  }

  def isPrivateTag(attr: Attribute): Boolean = {
    (attr.getTag.getGroup & 1) == 1
  }

  private val privateTagList = DicomUtil.findAll(rtplanTemplate, isPrivateTag _).map(_.getTag).toSet

  /**
    * Remove all private tags.
    */
  private def removePrivateTags(): Unit = {

    def isSeqAttr(attr: Attribute) = ValueRepresentation.isSequenceVR(attr.getVR)

    val alList = rtplanTemplate +: DicomUtil.findAll(rtplanTemplate, isSeqAttr _).asInstanceOf[Seq[SequenceAttribute]].flatMap(DicomUtil.alOfSeq)

    def remove(al: AttributeList): Unit = {
      privateTagList.map(tag => {
        if (al.get(tag) != null)
          al.remove(tag)
      })
    }

    alList.foreach(remove)
  }

  /**
    * Remove beams that are not one of the 4 required.
    */
  private def removeOtherBeams(): Unit = {

    // List of beam numbers that we need.  You gotta be on the list or you will be deleted.
    val requiredBeamNumberList = {
      val beamNameList = beamList.map(_.BeamName)
      val allBeams = DicomUtil.seqToAttr(rtplanTemplate, TagByName.BeamSequence)
      val requiredBeams = allBeams.filter(al => beamNameList.contains(nameOfBeam(al)))
      val beamNumberList = requiredBeams.map(al => al.get(TagByName.BeamNumber).getIntegerValues.head)
      beamNumberList
    }

    val seqList = DicomUtil.findAll(rtplanTemplate, attr => ValueRepresentation.isSequenceVR(attr.getVR)).asInstanceOf[Seq[SequenceAttribute]]

    def hasUnneededBeam(item: SequenceItem): Boolean = {
      val al = item.getAttributeList

      val ref = al.get(TagByName.ReferencedBeamNumber)
      val beam = al.get(TagByName.BeamNumber)

      val r = (ref != null) && (!requiredBeamNumberList.contains(ref.getIntegerValues.head))
      val b = (beam != null) && (!requiredBeamNumberList.contains(beam.getIntegerValues.head))

      r || b
    }

    def remove(seqAttr: SequenceAttribute): Unit = {
      val itemList = (0 until seqAttr.getNumberOfItems).map(i => seqAttr.getItem(i))
      itemList.filter(hasUnneededBeam).foreach(seqAttr.remove)
    }

    seqList.foreach(remove)
  }

  def makeRtplan(): AttributeList = {
    removePrivateTags()
    removeOtherBeams()
    setVariousAttributes()
    setDatesAndTimes()
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

    val mrp = new MakeRtPlan(PatientID = "Hiya", PatientName = "Lowe^Hiram", machineName = "JimMach", RTPlanLabel = "ThePlan", beamList = Seq(g000, g090, g180, g270))
    val plan = mrp.makeRtplan()

    val text = DicomUtil.attributeListToString(plan)
    println("===========================================================================")
    println(text)
    println("===========================================================================")
  }
}