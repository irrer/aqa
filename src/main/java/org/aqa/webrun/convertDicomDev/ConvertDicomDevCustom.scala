package org.aqa.webrun.convertDicomDev

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.SOPClass
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.Config
import org.aqa.db.Institution

import java.io.File
import java.util.Date

case object ConvertDicomDevCustom extends Logging {


  /**
   * Specifies the information needed to change a dev beam into a normal beam.
   *
   * @param dev      Beam whose image should be used.
   * @param template Beam delivered in normal/production mode whose metadata should be used as a template.
   */
  case class BeamConversion(dev: AttributeList, template: AttributeList) {}

  private def setTextList(al: AttributeList, tag: AttributeTag, valueList: Seq[String]): Unit = {
    val attr = AttributeFactory.newAttribute(tag)
    valueList.foreach(attr.addValue)
    al.put(attr)
  }

  private def setText(al: AttributeList, tag: AttributeTag, value: String): Unit = setTextList(al, tag, Seq(value))

  private def setText(al: AttributeList, tag: AttributeTag, value: Option[String]): Unit = if (value.isDefined) setTextList(al, tag, Seq(value.get))

  private def setInt(al: AttributeList, tag: AttributeTag, valueList: Seq[Int]): Unit = {
    val attr = AttributeFactory.newAttribute(tag)
    valueList.foreach(attr.addValue)
    al.put(attr)
  }

  private def setInt(al: AttributeList, tag: AttributeTag, value: Int): Unit = setInt(al, tag, Seq(value))

  private def setDouble(al: AttributeList, tag: AttributeTag, valueList: Iterable[Double]): Unit = {
    val attr = AttributeFactory.newAttribute(tag)
    valueList.foreach(attr.addValue)
    al.put(attr)
  }

  private def setDouble(al: AttributeList, tag: AttributeTag, value: Double): Unit = setDouble(al, tag, Seq(value))

  private def setTime(al: AttributeList, dateTag: AttributeTag, timeTag: AttributeTag, date: Date): Unit = {
    val dateAttr = AttributeFactory.newAttribute(dateTag)
    dateAttr.addValue(DicomUtil.dicomDateFormat.format(date))
    al.put(dateAttr)

    val timeAttr = AttributeFactory.newAttribute(timeTag)
    timeAttr.addValue(DicomUtil.dicomTimeFormat.format(date))
    al.put(timeAttr)
  }

  private def getContentDateTime(al: AttributeList): Date = {
    DicomUtil.getTimeAndDate(al, TagByName.ContentDate, TagByName.ContentTime).get
  }


  /**
   * Given the new beam captured in development mode, and a normal template beam, create a
   * new beam with the pixel of the dev beam, and with most of the metadata of the template
   * beam, but with new UIDs and date/times.
   *
   * @param dev               Beam delivered in development mode.
   * @param template          Beam delivered in normal production mode.
   * @param SeriesInstanceUID Series UID to be used for series.
   * @param StudyInstanceUID  Study UID to be used for series.
   * @param instanceDate      Date/time for this instance.
   * @param seriesDate        Date/time for the series.
   * @param studyDate         Date/time for the study.
   * @param machine           Mark new beam with this machine.  If not specified, then use the template's values.
   * @param rtplan            Point to this RTPLAN.
   */
  // @formatter:off
  private def setImageAttr(
                            dev: AttributeList,
                            template: AttributeList,
                            SeriesInstanceUID: String,
                            StudyInstanceUID: String,
                            instanceDate: Date,
                            seriesDate: Date,
                            studyDate: Date,
                            machine: Option[Machine],
                            rtplan: AttributeList)
  // @formatter:on
  : AttributeList = {

    val newAl = DicomUtil.clone(template)

    // ascertain values needed for new file

    // val rtplanBeam = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)(index)

    val SOPInstanceUID = UMROGUID.getUID

    val InstitutionName: Option[String] = {
      if (machine.isDefined) {
        val inst = Institution.get(machine.get.institutionPK).get
        Some(inst.getRealName)
      }
      else None
    }

    val RadiationMachineName: Option[String] = {
      if (machine.isDefined) {
        machine.get.getRealTpsId match {
          case Some(name) => Some(name)
          case _ => Some(machine.get.getRealId)
        }
      }
      else
        None
    }

    val DeviceSerialNumber: Option[String] = {
      if (machine.isDefined)
        machine.get.getRealDeviceSerialNumber
      else
        None
    }

    val FrameOfReferenceUID = UMROGUID.getUID

    val RTImagePosition: Seq[Double] = {

      val ImagePlanePixelSpacing = dev.get(TagByName.ImagePlanePixelSpacing).getDoubleValues
      val Rows = dev.get(TagByName.Rows).getIntegerValues.head
      val Columns = dev.get(TagByName.Columns).getIntegerValues.head

      val x = ((1 - Columns) / 2.0) * ImagePlanePixelSpacing(0)
      val y = ((Rows - 1) / 2.0) * ImagePlanePixelSpacing(1)

      Seq(x, y)
    }

    val planUid = Util.sopOfAl(rtplan)

    val ReferencedRTPlanSequence: SequenceAttribute = {
      val rps = AttributeFactory.newAttribute(TagByName.ReferencedRTPlanSequence).asInstanceOf[SequenceAttribute]
      val subAl = new AttributeList
      setText(subAl, TagByName.ReferencedSOPClassUID, SOPClass.RTPlanStorage)
      setText(subAl, TagByName.ReferencedSOPInstanceUID, planUid)
      rps.addItem(subAl)
      rps
    }

    // Use rows, columns, and pixel size from dev because image resolution can change with each delivery.
    val Rows = dev.get(TagByName.Rows).getIntegerValues.head
    val Columns = dev.get(TagByName.Columns).getIntegerValues.head
    val ImagePlanePixelSpacing = dev.get(TagByName.ImagePlanePixelSpacing).getDoubleValues

    val WindowCenter = dev.get(TagByName.WindowCenter).getDoubleValues.head
    val WindowWidth = dev.get(TagByName.WindowWidth).getDoubleValues.head
    val RescaleSlope = dev.get(TagByName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = dev.get(TagByName.RescaleIntercept).getDoubleValues.head

    val ContentTime = getContentDateTime(dev)

    // val PatientName = rtplan.get(TagByName.PatientName).getSingleStringValueOrEmptyString()
    // val PatientID = rtplan.get(TagByName.PatientID).getSingleStringValueOrEmptyString()

    // val ReferencedBeamNumber = rtplanBeam.get(TagByName.BeamNumber).getIntegerValues.head

    // -------- set various DICOM attributes --------

    setText(newAl, TagByName.MediaStorageSOPInstanceUID, SOPInstanceUID)
    // setText(newAl, TagByName.SpecificCharacterSet, "ISO_IR 192")
    setText(newAl, TagByName.SOPInstanceUID, SOPInstanceUID)

    setTime(newAl, TagByName.StudyDate, TagByName.StudyTime, studyDate)
    setTime(newAl, TagByName.SeriesDate, TagByName.SeriesTime, seriesDate)
    setTime(newAl, TagByName.AcquisitionDate, TagByName.AcquisitionTime, ContentTime)
    setTime(newAl, TagByName.ContentDate, TagByName.ContentTime, ContentTime)
    setTime(newAl, TagByName.InstanceCreationDate, TagByName.InstanceCreationTime, ContentTime)

    setText(newAl, TagByName.InstitutionName, InstitutionName)
    setText(newAl, TagByName.StationName, RadiationMachineName)
    // setText(newAl, TagByName.StudyDescription, "Machine QA")
    // setText(newAl, TagByName.OperatorsName, "NA")

    // setText(newAl, TagByName.ManufacturerModelName, "On-Board Imager")
    // setText(newAl, TagByName.PatientName, PatientName)
    // setText(newAl, TagByName.PatientID, PatientID)
    // setText(newAl, TagByName.PatientBirthDate, "18000101")
    // setText(newAl, TagByName.PatientBirthTime, "000000")
    // setText(newAl, TagByName.PatientSex, "O")
    setText(newAl, TagByName.DeviceSerialNumber, DeviceSerialNumber)
    // setText(newAl, TagByName.SoftwareVersions, "NA")
    // setText(newAl, TagByName.PatientPosition, "HFS")
    setText(newAl, TagByName.StudyInstanceUID, StudyInstanceUID)
    setText(newAl, TagByName.SeriesInstanceUID, SeriesInstanceUID)




    // setText(newAl, TagByName.StudyID, "NA")
    // setInt(newAl, TagByName.SeriesNumber, 0)
    setText(newAl, TagByName.FrameOfReferenceUID, FrameOfReferenceUID)

    setInt(newAl, TagByName.Rows, Rows)
    setInt(newAl, TagByName.Columns, Columns)
    setDouble(newAl, TagByName.ImagePlanePixelSpacing, ImagePlanePixelSpacing)

    setDouble(newAl, TagByName.WindowCenter, WindowCenter)
    setDouble(newAl, TagByName.WindowWidth, WindowWidth)
    setDouble(newAl, TagByName.RescaleSlope, RescaleSlope)
    setDouble(newAl, TagByName.RescaleIntercept, RescaleIntercept)

    // setText(newAl, TagByName.RTImageLabel, "NA")
    // setText(newAl, TagByName.RTImageDescription, "NA")
    // setIntList(newAl, TagByName.RTImageOrientation, Seq(1, 0, 0, 0, -1, 0))
    setText(newAl, TagByName.RadiationMachineName, RadiationMachineName)
    // setDouble(newAl, TagByName.GantryAngle, 0)
    newAl.put(ReferencedRTPlanSequence)
    setDouble(newAl, TagByName.RTImagePosition, RTImagePosition)
    // setInt(newAl, TagByName.ReferencedBeamNumber, ReferencedBeamNumber)

    if (false) {
      val devPix = dev.get(TagByName.PixelData)
      val templatePix = template.get(TagByName.PixelData)
      Trace.trace(devPix)
      Trace.trace(templatePix)
      val devShort = devPix.getShortValues
      val templateShort = templatePix.getShortValues
      Trace.trace(devShort.size)
      Trace.trace(templateShort.size)
      Trace.trace()
    }
    newAl.remove(TagByName.PixelData)
    newAl.put(dev.get(TagByName.PixelData))

    newAl
  }

  private def processSeries(beamConversionList: Seq[BeamConversion], machine: Machine, rtplan: AttributeList): Seq[AttributeList] = {

    val devList = beamConversionList.map(_.dev)
    val templateList = beamConversionList.map(_.template)

    def fmtAl(al: AttributeList): String = { // for debug only
      val pix = al.get(TagByName.PixelData).getShortValues
      var crc: Long = 0
      pix.foreach(p => {
        crc = (crc << 1) ^ (p & 0xff).toLong
      })
      "%08x".format(crc)
    }


    if (true) {
      val j = devList.map(fmtAl).mkString("\n    ==== ")
      Trace.trace(s"\n==== devFileList:\n    ==== $j")
    }

    if (true) {
      val j = templateList.map(f => fmtAl(f)).mkString("\n==== ")
      Trace.trace(s"\n==== templateFileList:\n    ==== $j")
    }
    println(s"====")

    // new UID for series
    val SeriesInstanceUID = UMROGUID.getUID

    // UID for study
    val StudyInstanceUID = templateList.head.get(TagByName.StudyInstanceUID).getSingleStringValueOrEmptyString

    // Use the content date of the first dev beam.  If there are no dev beams, then use the content
    // date of the first non-dev beam.  A beam is dev if it does not reference a beam.
    val seriesDate: Date = {
      def referencesBeam(al: AttributeList): Boolean = al.get(TagByName.ReferencedBeamNumber) != null

      val noBeam: Seq[Date] = devList.filterNot(referencesBeam).map(getContentDateTime).sorted
      val hasBeam: Seq[Date] = devList.filter(referencesBeam).map(getContentDateTime).sorted
      (noBeam ++ hasBeam).head
    }

    val studyDate = DicomUtil.getTimeAndDate(templateList.head, TagByName.StudyDate, TagByName.StudyTime).get

    val newAlList = beamConversionList.map(bc =>
      setImageAttr(
        dev = bc.dev,
        template = bc.template,
        SeriesInstanceUID = SeriesInstanceUID,
        StudyInstanceUID = StudyInstanceUID,
        instanceDate = getContentDateTime(bc.dev),
        seriesDate = seriesDate,
        studyDate = studyDate,
        machine = Some(machine),
        rtplan = rtplan
      ))


    newAlList

  }


  def mainFocalSpot(args: Array[String]): Unit = {

    Config.validate
    val machine = Machine.get(26).get

    Trace.trace("Starting")

    val rtplan = {
      val al = new AttributeList
      al.read("""D:\aqa\FocalSpot\FocalSpot_TX4\tool\rtplan.dcm""")
      al
    }


    //val devDir = new File("""D:\aqa\FocalSpot\FocalSpot_TX4\tool\dev""")
    val devDirList = Util.listDirFiles(new File("""D:\aqa\FocalSpot\FocalSpot_TX4\tool""")).filter(d => d.getName.matches("^dev[0-9]$"))
    val templateDir = new File("""D:\aqa\FocalSpot\FocalSpot_TX4\tool\template""")


    val templateList: Seq[AttributeList] = {
      val templateFileList = Util.listDirFiles(templateDir).filter(_.getName.endsWith(".dcm")).map(f => DicomFile(f))
      println
      templateFileList.foreach(df => println("==== template file: " + df.file.getAbsolutePath))
      val list = templateFileList.flatMap(_.attributeList).sortBy(getContentDateTime)
      list.toIndexedSeq
    }


    devDirList.foreach(devDir => {
      val devList: Seq[AttributeList] = {
        val devFileList = Util.listDirFiles(devDir).filter(_.getName.endsWith(".dcm")).map(f => DicomFile(f))
        println
        devFileList.foreach(df => println("==== dev file: " + df.file.getAbsolutePath))
        val list = devFileList.flatMap(_.attributeList).sortBy(getContentDateTime)
        list.toIndexedSeq
      }

      if (true) {
        templateList.foreach(al => println(s"==== Content: ${getContentDateTime(al)}   BeamRef: ${al.get(TagByName.ReferencedBeamNumber).getIntegerValues.head}"))
      }

      val beamConversionList: Seq[BeamConversion] = {
        val indexMap = Seq(1, 2, 7, 8, 5, 6, 3, 4).map(_ - 1)
        indexMap.indices.map(i => BeamConversion(devList(indexMap(i)), templateList(i)))
      }

      val newList = processSeries(beamConversionList, machine, rtplan)

      val outDir = new File(devDir, "out")
      Util.deleteFileTreeSafely(outDir)
      outDir.mkdirs()

      Trace.trace(s"Saving files to directory ${outDir.getAbsolutePath}")

      val Modality = newList.head.get(TagByName.Modality).getSingleStringValueOrEmptyString()

      def writeDicom(al: AttributeList, index: Int): Unit = {
        val dcmFile = new File(outDir, Modality + (index + 1) + ".dcm")
        Util.writeAttributeListToFile(al, dcmFile)
        val txtFile = new File(outDir, Modality + (index + 1) + ".txt")
        Util.writeFile(txtFile, DicomUtil.attributeListToString(al))
      }

      newList.zipWithIndex.foreach(ai => writeDicom(ai._1, ai._2))
      Trace.trace()

    })
    Thread.sleep(1000)
    Trace.trace("Done")
    System.exit(0)
  }


  def main(args: Array[String]): Unit = {

    def fileToAl(file: File): AttributeList = {
      val al = new AttributeList
      al.read(file)
      al
    }

    Config.validate
    val machine = Machine.get(26).get

    Trace.trace("Starting")

    val rtplan = {
      val al = new AttributeList
      al.read("""D:\aqa\sym\6X_symPROCESS\RP.1.2.246.352.71.5.427549902257.1092564.20240208125522.dcm""")
      al
    }

    val dirList = {
      val d6 = new File("""D:\aqa\sym\6X_symPROCESS""")
      val list6 = Util.listDirFiles(d6).filter(_.getName.matches("Sym[0-9][0-9]"))

      val d16 = new File("""D:\aqa\sym\16x_symPROCESS""")
      val list16 = Util.listDirFiles(d16).filter(_.getName.matches("16xSym[0-9][0-9]"))

      list6 ++ list16
    }

    def doDir(dir: File): Unit = {

      val list = Util.listDirFiles(dir).filter(_.getName.matches("^SID[0-9]+_NEW.dcm$")).map(_.getName.replace("_NEW.dcm", "")).sorted
      if (list.isEmpty) {
        Trace.trace("Ignoring " + dir.getAbsolutePath)
      }
      else {
        val devList = list.map(name => new File(dir, name + ".dcm")).filter(_.canRead).map(fileToAl)
        val templateList = list.map(name => new File(dir, name + "_NEW.dcm")).filter(_.canRead).map(fileToAl)

        if (devList.isEmpty || (devList.size != templateList.size)) {
          Trace.trace("Ignoring badness in " + dir.getAbsolutePath)
        }
        else {

          val beamConversionList = devList.zip(templateList).map(dt => BeamConversion(dt._1, dt._2))

          val processedList = processSeries(beamConversionList, machine, rtplan)

          val outDir = new File(dir, "PROCESSED")
          Util.deleteFileTreeSafely(outDir)
          outDir.mkdirs()

          def writeDicom(name: String, al: AttributeList): Unit = {
            val dcmFile = new File(outDir, name + ".dcm")
            Util.writeAttributeListToFile(al, dcmFile)
            val txtFile = new File(outDir, name + ".txt")
            Util.writeFile(txtFile, DicomUtil.attributeListToString(al))
            Trace.trace("wrote " + dcmFile.getAbsolutePath)
          }

          list.zip(processedList).foreach(nameAl => writeDicom(nameAl._1, nameAl._2))
        }
      }
    }

    dirList.foreach(doDir)

    Thread.sleep(1000)
    Trace.trace("Done")
    System.exit(0)
  }


}
