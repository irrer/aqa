package org.aqa.simpleRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.DicomDict
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.VarianPrivateTag
import org.aqa.db.Machine

import java.io.File
import java.util.Date

/**
  *
  * @param PatientID Patient ID to use.
  * @param PatientName Name of patient.
  * @param machine Name of machine.
  * @param RTPlanLabel Plan label ID to use.
  * @param beamList List of beam specifications.
  */
class MakeRtPlan(
    PatientID: String,
    PatientName: String,
    machine: Machine,
    RTPlanLabel: String,
    beamList: Seq[SimpleBeamSpecification]
) extends Logging {

  /** Name of application writing DICOM files. */
  private val sourceApplication = "AQA Simple RTPlan"

  /** Used for DICOM dates and times. */
  private val now = new Date()

  /**
    * Given a beam, return its name.
    * @param beamAl Beam sequence item.
    * @return Beam name.
    */
  private def numberOfBeam(beamAl: AttributeList): Int = {
    val number = DicomUtil.findAllSingle(beamAl, TagByName.BeamNumber).head.getIntegerValues.head
    number
  }

  private def nameOfBeam(beamAl: AttributeList): String = {
    val name = DicomUtil.findAllSingle(beamAl, TagByName.BeamName).head.getSingleStringValueOrEmptyString()
    name
  }

  private def setNominalBeamEnergy(NominalBeamEnergy: Attribute, energy: Double): Unit = {
    NominalBeamEnergy.removeValues()
    NominalBeamEnergy.addValue(energy)
  }

  private def setSourceToSurfaceDistance(SourceToSurfaceDistance: Attribute, distance_mm: Double): Unit = {
    SourceToSurfaceDistance.removeValues()
    SourceToSurfaceDistance.addValue(distance_mm)
  }

  /**
    * Set the jaws according to what the user requested.
    *
    * @param beamTemplate Beam to change.
    * @param deviceTypeList Which part of beam to change.
    * @param d1 Lower value (1)
    * @param d2 Upper value (1)
    */
  private def setLeafJawPositions(beamTemplate: AttributeList, deviceTypeList: Seq[String], d1: Double, d2: Double): Unit = {
    val positionSeqList = {
      val atSeq = DicomUtil.findAllSingle(beamTemplate, TagByName.BeamLimitingDevicePositionSequence).asInstanceOf[IndexedSeq[SequenceAttribute]]
      val list = atSeq.flatMap(DicomUtil.alOfSeq)
      list
    }

    val xTypeList = Seq("X", "ASYMX", "MLCX")

    def typeOk(ps: AttributeList): Boolean = {
      val devType = ps.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString()
      deviceTypeList.contains(devType)
    }

    val jawSeq = positionSeqList.filter(typeOk)

    def setJaw(ps: AttributeList): Unit = {
      val LeafJawPositions = ps.get(TagByName.LeafJawPositions)
      LeafJawPositions.removeValues()
      LeafJawPositions.addValue(d1)
      LeafJawPositions.addValue(d2)

      // set the type according to whether it is X or Y, and whether it is symmetric or not.
      val RTBeamLimitingDeviceType = ps.get(TagByName.RTBeamLimitingDeviceType)
      val dt = RTBeamLimitingDeviceType.getSingleStringValueOrEmptyString()
      val symmetric = (-d1) == d2
      val isX = xTypeList.contains(dt)
      val newType = (isX, symmetric) match {
        case (true, true)   => "X" // X, symmetric
        case (true, false)  => "ASYMX" // X, asymmetric
        case (false, true)  => "Y" // Y, symmetric
        case (false, false) => "ASYMY" // Y, asymmetric
      }

      def changeDeviceType(attr: Attribute): Unit = {
        attr.removeValues()
        attr.addValue(newType)
      }

      val deviceTypeList = DicomUtil.findAllSingle(beamTemplate, TagByName.RTBeamLimitingDeviceType).filter(_.getSingleStringValueOrEmptyString().equals(dt))
      deviceTypeList.foreach(changeDeviceType)
    }

    jawSeq.foreach(setJaw)
  }

  private def refBeamSeq(rtplan: AttributeList, beamNumber: Int): AttributeList = {
    val all = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).flatMap(al => DicomUtil.seqToAttr(al, TagByName.ReferencedBeamSequence))
    val ref = all.find(al => al.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == beamNumber)
    ref.get
  }

  private def setupBeam(rtplan: AttributeList, beamSpecification: SimpleBeamSpecification): Unit = {

    val beamAl = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence).find(b => numberOfBeam(b) == beamSpecification.BeamNumber).get
    val beamRefAl = refBeamSeq(rtplan, beamSpecification.BeamNumber)

    // Find all references to beam energy and set them to the specified level.
    DicomUtil.findAllSingle(beamAl, TagByName.NominalBeamEnergy).foreach(nbe => setNominalBeamEnergy(nbe, beamSpecification.NominalBeamEnergy))

    // Find all references to SourceToSurfaceDistance and set them to the specified level.
    DicomUtil.findAllSingle(beamAl, TagByName.SourceToSurfaceDistance).foreach(nbe => setSourceToSurfaceDistance(nbe, beamSpecification.SourceToSurfaceDistance))

    def setRBS(tag: AttributeTag, value: Double): Unit = {
      beamRefAl.remove(tag)
      val attr = AttributeFactory.newAttribute(tag, ValueRepresentation.DS)
      attr.addValue(value)
      beamRefAl.put(attr)
    }

    // set the X and Y jaw values
    setLeafJawPositions(beamAl, Seq("X", "ASYMX"), beamSpecification.X1_mm, beamSpecification.X2_mm)
    setLeafJawPositions(beamAl, Seq("Y", "ASYMY"), beamSpecification.Y1_mm, beamSpecification.Y2_mm)

    // set the same jaw positions for the corresponding port film beam.  Also set the same SourceToSurfaceDistance.
    def setPortfilmAl(): Unit = {
      val beamName = nameOfBeam(beamAl)
      //noinspection SpellCheckingInspection
      val tp = DicomUtil
        .seqToAttr(rtplan, TagByName.BeamSequence)
        .filter(b => nameOfBeam(b).equals(beamName) && b.get(TagByName.TreatmentDeliveryType).getSingleStringValueOrEmptyString().equals("TRMT_PORTFILM"))

      tp.foreach(t => setLeafJawPositions(t, Seq("X", "ASYMX"), beamSpecification.X1_mm, beamSpecification.X2_mm))
      tp.foreach(t => setLeafJawPositions(t, Seq("Y", "ASYMY"), beamSpecification.Y1_mm, beamSpecification.Y2_mm))
      tp.foreach(t => DicomUtil.findAllSingle(t, TagByName.SourceToSurfaceDistance).foreach(nbe => setSourceToSurfaceDistance(nbe, beamSpecification.SourceToSurfaceDistance)))
    }
    setPortfilmAl()

    logger.info("Set up beam parameters: " + beamSpecification)

    setRBS(TagByName.BeamMeterset, beamSpecification.BeamMeterset)
    setRBS(VarianPrivateTag.MaximumTreatmentTime, beamSpecification.MaximumTreatmentTime_min)
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

  /*
  private def setMachineForBeams(al: AttributeList, machine: Machine): Unit = {
    val rtimage = 0
    val machineID = machine.getRealTpsId.get
    val machineDeviceSerialNumber = machine.getRealDeviceSerialNumber.get
    val internalId = 5
    setAll(al, TagByName.TreatmentMachineName, machineID)
  }
   */

  private def setVariousAttributes(al: AttributeList): Unit = {
    setAll(al, TagByName.PatientID, PatientID)

    setAll(al, TagByName.PatientName, PatientName)

    setAll(al, TagByName.PatientSex, "O")

  }

  private def setRtplanAttributes(al: AttributeList): Unit = {
    setAll(al, TagByName.RTPlanLabel, RTPlanLabel)

    val beamAlList = DicomUtil.seqToAttr(al, TagByName.BeamSequence)
    val tpsId = machine.getRealTpsId.get
    beamAlList.foreach(beamAl => setAll(beamAl, TagByName.TreatmentMachineName, tpsId))

    val machineDeviceSerialNumber = machine.getRealDeviceSerialNumber.get
    beamAlList.foreach(beamAl => setAll(beamAl, TagByName.DeviceSerialNumber, machineDeviceSerialNumber))
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

  private def makeRtplan(rtplan: AttributeList): Unit = {
    setVariousAttributes(rtplan)
    setRtplanAttributes(rtplan)
    setDatesAndTimes(rtplan)
    makeNewUIDs(rtplan)

    beamList.foreach(b => setupBeam(rtplan, b))
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
  def makeZipWithSupportingFiles(csvText: String): ModifiedPlan = {
    val toZipOutputStream = new FileUtil.ToZipOutputStream

    val templateFiles = new TemplateFiles

    val rtplan = templateFiles.ofModality(modality = "RTPLAN").head.fileToDicom()

    makeRtplan(rtplan)
    val rtplanText = DicomUtil.attributeListToString(rtplan)
    toZipOutputStream.writeDicom(rtplan, path = "RTPLAN.dcm", sourceApplication)
    toZipOutputStream.write(csvText.getBytes(), "Summary.csv")
    toZipOutputStream.write(rtplanText.getBytes(), "RTPLAN.txt")
    logger.info("Made RTPLAN file " + rtplan.get(TagByName.RTPlanLabel).getSingleStringValueOrEmptyString())
    makeRtstruct(toZipOutputStream, templateFiles)
    makeRTIMAGE(toZipOutputStream, templateFiles)
    makeCT(toZipOutputStream, templateFiles)

    val data = toZipOutputStream.finish()
    Trace.trace("\nModified plan -------------------------------------------\n" + rtplanText + "\nEnd of modified plan -------------------------------------------")
    ModifiedPlan(rtplanText, Util.sopOfAl(rtplan), data)
  }

}

object MakeRtPlan {
  def main(args: Array[String]): Unit = {

    if (true) {
      val f = new File("""src\main\resources\static\rtplan\rtplanPhase2Millenium.dcm""")
      val al = new AttributeList
      al.read(f)
      val t = DicomUtil.attributeListToString(al)
      println(t)

      DicomUtil
        .findAll(al, _ => true)
        .foreach(attr => {
          println(DicomDict.dict.getNameFromTag(attr.getTag) + " : " + DicomDict.dict.getInformationEntityFromTag(attr.getTag))
        })

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
