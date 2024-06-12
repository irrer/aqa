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

  private def setDouble(al: AttributeList, tag: AttributeTag, valueList: Seq[Double]): Unit = {
    val attr = AttributeFactory.newAttribute(tag)
    valueList.foreach(attr.addValue)
    al.put(attr)
  }

  private def setDouble(al: AttributeList, tag: AttributeTag, value: Double): Unit = setDouble(al, tag, Seq(value))

  /*
  private def getRtPlanUid(rtimageList: Seq[AttributeList], rtplan: Option[AttributeList]): String = {
    if (rtplan.isDefined)
      Util.sopOfAl(rtplan.get)
    else {
      val j = rtimageList.flatMap(Util.getRtplanSop).groupBy(uid => uid).maxBy(_._2.length)._1
      ???

    }
  }
   */

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

  private val minute_ms = 60 * 1000
  private val imageTimeOffset_ms = minute_ms
  private val seriesTimeOffset_ms = minute_ms * 10
  private val hour_ms = 60 * minute_ms

  /**
   * Set the dates and times for an image that vary for each image.
   *
   * @param al   Image.
   * @param date New date.
   */
  /*
  private def setImageTimes(al: AttributeList, date: Date): Unit = {

    val pairList = Seq(
      (TagByName.AcquisitionDate, TagByName.AcquisitionTime),
      (TagByName.ContentDate, TagByName.ContentTime),
      (TagByName.InstanceCreationDate, TagByName.InstanceCreationTime)
    )

    pairList.foreach(pair => setTime(al, pair._1, pair._2, date))
  }
  */

  /**
   * Set all of the date + time values for the given series.
   *
   * @param alList Images for series sorted by time.
   * @param index  Series number.
   */
  /*
  private def setPerImageTime(alList: Seq[AttributeList], index: Int): Date = {
    // images sorted chronologically.
    val firstImageTime_ms = {
      val first = getContentDateTime(alList.head)
      val t = first.getTime + ((hour_ms * 4) - 1)
      val s = t - (t % hour_ms)
      val u = s + (seriesTimeOffset_ms * index)
      u
    }

    val firstImageDate = new Date(firstImageTime_ms)

    alList.indices.foreach(i => setImageTimes(alList(i), new Date(firstImageTime_ms + (i * imageTimeOffset_ms))))

    firstImageDate
  }
  */


  /**
   * Given the new beam captured in development mode, and a normal template beam, create a
   * new beam with the pixel of the dev beam, and with most of the metadata of the templat
   * beam, but with new UIDs and date/times.
   *
   * @param dev               Beam delivered in development mode.
   * @param template          Beam delivered in normal production mode.
   * @param SeriesInstanceUID Series UID to be used for series.
   * @param index             Index of this beam (0 is the first one).
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
                            index: Int,
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

    if (System.currentTimeMillis() < 0) {
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
    newAl.put(dev.get(TagByName.PixelData))

    newAl
  }

  private def processSeries(dir: File, index: Int, machine: Machine, rtplan: AttributeList): Unit = {
    Trace.trace(s"Reading directory ${dir.getAbsolutePath} ...")
    val j = Util.listDirFiles(dir).foreach(d => println(s"====== $d"))
    val devFileList = Util.listDirFiles(dir).filter(_.getName.endsWith(".dcm")).filter(_.getName.endsWith("_NEW.dcm")).map(f => DicomFile(f))
    val templateDir = new File("""D:\aqa\FocalSpot\FocalSpot_TX4\Focal_SpotPROCESS\FocalSpot_GoodHeaders""")
    // val templateFileList = Util.listDirFiles(templateDir).filter(_.getName.endsWith(".dcm")).map(f => DicomFile(f))
    val templateFileList = devFileList // Util.listDirFiles(templateDir).filter(_.getName.endsWith(".dcm")).map(f => DicomFile(f))

    def fmtAl(dicomFile: DicomFile): String = { // for debug only
      val pix = dicomFile.attributeList.get.get(TagByName.PixelData).getShortValues
      var crc: Long = 0
      pix.foreach(p => {
        crc = (crc << 1) ^ (p & 0xff).toLong
      })
      dicomFile.file.getAbsolutePath + " " + "%08x".format(crc)
    }

    if (devFileList.nonEmpty) {

      if (true) {
        val j = devFileList.map(f => fmtAl(f)).mkString("\n    ==== ")
        Trace.trace(s"\n==== devFileList:\n    ==== $j")
      }

      val devList = devFileList.flatMap(_.attributeList).sortBy(al => getContentDateTime(al).getTime)

      val templateList = templateFileList.flatMap(_.attributeList).sortBy(al => getContentDateTime(al).getTime)
      if (true) {
        val j = templateFileList.map(f => fmtAl(f)).mkString("\n==== ")
        Trace.trace(s"\n==== templateFileList:\n    ==== $j")
      }
      println(s"====")
      println(s"====")


      val SeriesInstanceUID = UMROGUID.getUID
      val StudyInstanceUID = UMROGUID.getUID

      val seriesDate = getContentDateTime(devList.head)

      val studyDate = DicomUtil.getTimeAndDate(templateList(index), TagByName.StudyDate, TagByName.StudyTime).get

      val newAlList = devList.indices.map(index =>
        setImageAttr(
          dev = devList(index),
          template = templateList(index),
          SeriesInstanceUID = SeriesInstanceUID,
          index = index,
          StudyInstanceUID = StudyInstanceUID,
          instanceDate = DicomUtil.getTimeAndDate(devList(index), TagByName.ContentDate, TagByName.ContentTime).get,
          seriesDate = seriesDate,
          studyDate = studyDate,
          machine = Some(machine),
          rtplan = rtplan
        ))


      val outDir = new File(dir, "out2")
      Util.deleteFileTreeSafely(outDir)
      outDir.mkdirs()

      Trace.trace(s"Saving files to directory ${outDir.getAbsolutePath}")

      val Modality = newAlList.head.get(TagByName.Modality).getSingleStringValueOrEmptyString()

      def writeDicom(al: AttributeList, index: Int): Unit = {
        val dcmFile = new File(outDir, Modality + (index + 1) + ".dcm")
        Util.writeAttributeListToFile(al, dcmFile)
        val txtFile = new File(outDir, Modality + (index + 1) + ".txt")
        Util.writeFile(txtFile, DicomUtil.attributeListToString(al))
      }

      newAlList.zipWithIndex.foreach(ai => writeDicom(ai._1, ai._2))
      Trace.trace()
    }

  }

  def main(args: Array[String]): Unit = {

    Config.validate
    val machine = Machine.get(26).get

    Trace.trace("Starting")

    val dirList =
      Util.listDirFiles(new File("""D:\aqa\FocalSpot\FocalSpot_TX4\Focal_SpotPROCESS""")).filter(_.isDirectory).filter(_.getName.matches("^FocalSpot.$")).sortBy(_.getName)

    val rtplan = {
      val al = new AttributeList
      al.read("""D:\aqa\FocalSpot\FocalSpot_TX4\Focal_SpotPROCESS\RP.1.2.246.352.71.5.427549902257.1092044.20240207145418.dcm""")
      al
    }

    dirList.indices.foreach(i => processSeries(dirList(i), i, machine, rtplan))

    Thread.sleep(1000)
    Trace.trace("Done")
    System.exit(0)
  }


}
