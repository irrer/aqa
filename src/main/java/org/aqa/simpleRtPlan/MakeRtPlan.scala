package org.aqa.simpleRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.SequenceItem
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.util.UMROGUID
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Machine

import java.io.File
import java.util.Date

/**
  *
  * @param PatientID Patient ID to use.
  * @param PatientName Name of patient.
  * @param machine Name of machine.
  * @param RTPlanLabel Plan label ID to use.
  * @param ToleranceTableLabel Tolerance table to use.
  * @param beamList List of beam specifications.
  */
class MakeRtPlan(
    PatientID: String,
    PatientName: String,
    machine: Machine,
    RTPlanLabel: String,
    ToleranceTableLabel: String,
    beamList: Seq[SimpleBeamSpecification]
) extends Logging {

  /** Name of application writing DICOM files. */
  private val sourceApplication = "AQA Simple RTPlan"

  /** Setting this to true will cause all beams to be kept, even those that are not of the main 4. */
  private val keepOtherBeams = true

  /** Setting this to true will cause all private tags to be kept. */
  private val keepPrivateTags = true

  /** Used for DICOM dates and times. */
  private val now = new Date()

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

  /**
    * Set the jaws according to what the user requested.
    *
    * @param beamTemplate Beam to change.
    * @param deviceTypeList Which part of beam to change.
    * @param size_mm Dimension of beam.
    */
  private def setLeafJawPositions(beamTemplate: AttributeList, deviceTypeList: Seq[String], d1: Double, d2: Double): Unit = {
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
      LeafJawPositions.addValue(-d1)
      LeafJawPositions.addValue(d2)
    }

    positionSeqList.filter(ps => typeOk(ps)).foreach(setJaw)
  }

  private def setupBeam(BeamSequence: Seq[AttributeList], beamSpecification: SimpleBeamSpecification): Unit = {

    val beamTemplate = BeamSequence.find(b => nameOfBeam(b).equalsIgnoreCase(beamSpecification.BeamName)).get

    // Find all references to beam energy and set them to the specified level.
    DicomUtil.findAllSingle(beamTemplate, TagByName.NominalBeamEnergy).foreach(nbe => setNominalBeamEnergy(nbe, beamSpecification.NominalBeamEnergy))

    // set the X and Y jaw values
    setLeafJawPositions(beamTemplate, Seq("X", "ASYMX"), beamSpecification.X1_mm , beamSpecification.X2_mm  )
    setLeafJawPositions(beamTemplate, Seq("Y", "ASYMY"), beamSpecification.Y1_mm, beamSpecification.Y2_mm )
    logger.info("Set up beam parameters: " + beamSpecification)
  }

  /**
    * Set all attributes with the given tag to the given value.
    * @param tag Tag to find.
    * @param value New value.
    */
  private def setAll(al: AttributeList, tag: AttributeTag, value: String): Unit = {
    val list = DicomUtil.findAllSingle(al, tag)

    list.foreach(attr => {
      attr.removeValues()
      attr.addValue(value)
    })
  }

  private def setMachineForBeams(al: AttributeList, machine: Machine): Unit = {
    val rtimage = 0
    val machineID = machine.getRealId
    val machineDeviceSerialNumber = machine.getRealDeviceSerialNumber.get
    val internalId = 5
    setAll(al, TagByName.TreatmentMachineName, machineID)


  }

  private def setVariousAttributes(al: AttributeList): Unit = {
    setAll(al, TagByName.PatientID, PatientID)

    setAll(al, TagByName.PatientName, PatientName)

    setAll(al, TagByName.PatientSex, "O")
    setAll(al, TagByName.RTPlanLabel, RTPlanLabel)
    setAll(al, TagByName.ToleranceTableLabel, ToleranceTableLabel)

    setAll(al, TagByName.Manufacturer, "AQA")
    setAll(al, TagByName.ManufacturerModelName, "Simple Plan")
    setAll(al, TagByName.DeviceSerialNumber, "001")
    setAll(al, TagByName.SoftwareVersions, "0.0.1")
  }

  private def setDatesAndTimes(al: AttributeList): Unit = {
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

    DicomUtil.findAll(al, setDates _)
    DicomUtil.findAll(al, setTimes _)

    setAll(al, TagByName.PatientBirthDate, "18000101")
    setAll(al, TagByName.PatientBirthTime, "000000")
  }

  /** Map of which UIDs were changed and their replacements. */
  private val UIDSet = scala.collection.mutable.HashMap[String, String]()

  private def getReplacementUID(oldUID: String): String =
    UIDSet.synchronized {
      if (!UIDSet.contains(oldUID)) {
        val newUID = UMROGUID.getUID
        UIDSet.put(oldUID, newUID)
      }
      UIDSet(oldUID)
    }

  private def makeNewUIDs(al: AttributeList): Unit = {
    val keySet = Config.ToBeAnonymizedList.keySet.filter(tag => ValueRepresentation.isUniqueIdentifierVR(DicomUtil.dictionary.getValueRepresentationFromTag(tag)))
    val attrList = DicomUtil.findAll(al, keySet)
    def replace(at: Attribute): Unit = {
      val uid = getReplacementUID(at.getSingleStringValueOrEmptyString)
      at.removeValues()
      at.addValue(uid)
    }
    attrList.foreach(at => replace(at))
  }

  private def isPrivateTag(attr: Attribute): Boolean = {
    (attr.getTag.getGroup & 1) == 1
  }

  /**
    * Remove all private tags.
    */
  private def removePrivateTags(al: AttributeList): Unit = {

    if (keepPrivateTags) {
      logger.info("Keeping private tags.")
    } else {
      val privateTagList = DicomUtil.findAll(al, isPrivateTag _).map(_.getTag).toSet

      def isSeqAttr(attr: Attribute) = ValueRepresentation.isSequenceVR(attr.getVR)

      val alList = al +: DicomUtil.findAll(al, isSeqAttr _).asInstanceOf[Seq[SequenceAttribute]].flatMap(DicomUtil.alOfSeq)

      def remove(al: AttributeList): Unit = {
        privateTagList.map(tag => {
          if (al.get(tag) != null)
            al.remove(tag)
        })
      }

      val privateAttributeList = DicomUtil.findAll(al, isPrivateTag _)
      logger.info("Removing private tags.  Count: " + privateAttributeList.size)

      alList.foreach(remove)
    }
  }

  /**
    * Remove beams that are not one of the 4 required.
    */
  private def removeOtherBeams(rtplan: AttributeList): Unit = {
    if (keepOtherBeams) {
      logger.info("Keeping all beams.")
    } else {
      // List of beam numbers that we need.  You gotta be on the list or you will be deleted.
      val requiredBeamNumberList = {
        val beamNameList = beamList.map(_.BeamName)
        val allBeams = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
        val requiredBeams = allBeams.filter(al => beamNameList.contains(nameOfBeam(al)))
        val beamNumberList = requiredBeams.map(al => al.get(TagByName.BeamNumber).getIntegerValues.head)
        beamNumberList
      }

      val seqList = DicomUtil.findAll(rtplan, attr => ValueRepresentation.isSequenceVR(attr.getVR)).asInstanceOf[Seq[SequenceAttribute]]
      logger.info("Removing extra beams and keeping the main 4.")

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
        val removeList = itemList.filter(hasUnneededBeam)
        logger.info("Removing non-main beams.  Count: " + removeList.size)
        removeList.foreach(seqAttr.remove)
      }

      seqList.foreach(remove)
    }
  }

  private def makeRtplan(rtplan: AttributeList): Unit = {
    removePrivateTags(rtplan)
    removeOtherBeams(rtplan)
    setVariousAttributes(rtplan)
    setDatesAndTimes(rtplan)
    makeNewUIDs(rtplan)

    val BeamSequence = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    beamList.foreach(b => setupBeam(BeamSequence, b))
  }

  /**
    * Modify the RTSTRUCT templates and write them to the stream.
    *
    * @param toZipOutputStream Write DICOM here.
    * @param templateFiles List of configured files.
    */
  private def makeRtstruct(toZipOutputStream: FileUtil.ToZipOutputStream, templateFiles: TemplateFiles): Unit = {
    val rtStructList = templateFiles.fileList.filter(_.modality.equals("RTSTRUCT"))

    def make(rtstructRef: TemplateFileRef, index: Int): Unit = {
      val rtstruct = rtstructRef.fileToDicom()
      removePrivateTags(rtstruct)
      setVariousAttributes(rtstruct)
      setDatesAndTimes(rtstruct)
      makeNewUIDs(rtstruct)
      val suffix = if (rtStructList.size == 1) "" else "_" + (index + 1)
      toZipOutputStream.writeDicom(rtstruct, "RTSTRUCT" + suffix + ".dcm", sourceApplication)

      val description = {
        val list = Seq(TagByName.SOPInstanceUID, TagByName.StructureSetLabel, TagByName.ROIName, TagByName.ROIObservationLabel)
        val textList = list.flatMap(tag => { DicomUtil.findAllSingle(rtstruct, tag).map(_.getSingleStringValueOrEmptyString()) })
        textList.mkString("  ")
      }

      logger.info("Made RTSTRUCT file " + description)
    }

    rtStructList.zipWithIndex.foreach(ri => make(ri._1, ri._2))
  }

  /**
    * Modify the CT templates and write them to the stream.
    *
    * @param toZipOutputStream Write DICOM here.
    * @param templateFiles List of configured files.
    */
  private def makeCT(toZipOutputStream: FileUtil.ToZipOutputStream, templateFiles: TemplateFiles): Unit = {
    val ctList = templateFiles.fileList.filter(_.modality.equals("CT")).sortBy(_.file.getName)

    def make(ctRef: TemplateFileRef, index: Int): Unit = {
      val ct = ctRef.fileToDicom()
      removePrivateTags(ct)
      setVariousAttributes(ct)
      setDatesAndTimes(ct)
      makeNewUIDs(ct)
      val suffix = if (ctList.size == 1) "" else "_" + (index + 1).formatted("%03d")
      toZipOutputStream.writeDicom(ct, "CT" + suffix + ".dcm", sourceApplication)
      logger.info("Made CT slice " + ct.get(TagByName.ImagePositionPatient).getDoubleValues.map(Util.fmtDbl).mkString("  "))
    }

    ctList.zipWithIndex.foreach(ri => make(ri._1, ri._2))
  }

  /**
    * Modify the RTIMAGE templates and write them to the stream.
    *
    * @param toZipOutputStream Write DICOM here.
    * @param templateFiles List of configured files.
    */
  private def makeRTIMAGE(toZipOutputStream: FileUtil.ToZipOutputStream, templateFiles: TemplateFiles): Unit = {
    val rtimageList = templateFiles.fileList.filter(_.modality.equals("RTIMAGE"))

    def make(rtimageRef: TemplateFileRef, index: Int): Unit = {
      val rtimage = rtimageRef.fileToDicom()
      removePrivateTags(rtimage)
      setVariousAttributes(rtimage)
      setDatesAndTimes(rtimage)
      makeNewUIDs(rtimage)
      val suffix = if (rtimageList.size == 1) "" else "_" + (index + 1)
      toZipOutputStream.writeDicom(rtimage, "RTIMAGE" + suffix + ".dcm", sourceApplication)
      val description = "Beam number " + rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head + " : " +
        DicomUtil.findAllSingle(rtimage, TagByName.LeafJawPositions).map(_.getDoubleValues.mkString(", ")).mkString("    ")
      logger.info("Made RTIMAGE " + description)
    }

    rtimageList.zipWithIndex.foreach(ri => make(ri._1, ri._2))
  }

  /**
    * Make the RTPLAN and the files that it references, including RTSTRUCT, RTIMAGE, and CT.
    *
    * The point of the supporting files is to allow the user to bring this into
    * another system for viewing.
    *
    * @return A text version of the RTPLAN and a zipped byte array of all of the DICOM files.
    */
  def makeZipWithSupportingFiles(): ModifiedPlan = {
    val toZipOutputStream = new FileUtil.ToZipOutputStream

    val templateFiles = new TemplateFiles

    val rtplan = templateFiles.ofModality(modality = "RTPLAN").head.fileToDicom()

    makeRtplan(rtplan)
    toZipOutputStream.writeDicom(rtplan, path = "RTPLAN.dcm", sourceApplication)
    logger.info("Made RTPLAN file " + rtplan.get(TagByName.RTPlanLabel).getSingleStringValueOrEmptyString())
    makeRtstruct(toZipOutputStream, templateFiles)
    makeRTIMAGE(toZipOutputStream, templateFiles)
    makeCT(toZipOutputStream, templateFiles)

    val data = toZipOutputStream.finish()
    val text = DicomUtil.attributeListToString(rtplan)
    ModifiedPlan(text, Util.sopOfAl(rtplan), data)
  }

}

object MakeRtPlan {
  def main(args: Array[String]): Unit = {

    if (true) {
      val f = new File("""D:\tmp\maggie\33529_RTPLAN_1__0000_1_X.DCM""")
      val al = new AttributeList
      al.read(f)
      val t = DicomUtil.attributeListToString(al)
      println(t)
      System.exit(99)
    }

    /*
    val g000 = SimpleSpecification(GantryAngle_deg = 0, BeamName = Config.SimpleRtplanBeamNameG000, X_mm = 25, Y_mm = 26, NominalBeamEnergy = 6)
    val g090 = SimpleSpecification(GantryAngle_deg = 90, BeamName = Config.SimpleRtplanBeamNameG090, X_mm = 35, Y_mm = 36, NominalBeamEnergy = 6)
    val g180 = SimpleSpecification(GantryAngle_deg = 180, BeamName = Config.SimpleRtplanBeamNameG180, X_mm = 45, Y_mm = 46, NominalBeamEnergy = 6)
    val g270 = SimpleSpecification(GantryAngle_deg = 270, BeamName = Config.SimpleRtplanBeamNameG270, X_mm = 55, Y_mm = 56, NominalBeamEnergy = 6)

    val mrp =
      new MakeRtPlan(PatientID = "$AQA_Simple", PatientName = "$AQA_Simple", machine = ???, RTPlanLabel = "AQA Simple", ToleranceTableLabel = "PELVIS", beamList = Seq(g000, g090, g180, g270))
    val modifiedPlan = mrp.makeZipWithSupportingFiles()

    println("===========================================================================")
    println(modifiedPlan.rtplanText)
    println("===========================================================================")

    val outFile = new File("target/simple.zip")
    Util.writeBinaryFile(outFile, modifiedPlan.zippedContent)
    println("Wrote file " + outFile.getAbsolutePath)
    */
  }
}
