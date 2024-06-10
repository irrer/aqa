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

  private def setIntList(al: AttributeList, tag: AttributeTag, valueList: Seq[Int]): Unit = {
    val attr = AttributeFactory.newAttribute(tag)
    valueList.foreach(attr.addValue)
    al.put(attr)
  }

  private def setInt(al: AttributeList, tag: AttributeTag, value: Int): Unit = setIntList(al, tag, Seq(value))

  private def setDoubleList(al: AttributeList, tag: AttributeTag, valueList: Seq[Double]): Unit = {
    val attr = AttributeFactory.newAttribute(tag)
    valueList.foreach(attr.addValue)
    al.put(attr)
  }

  private def setDouble(al: AttributeList, tag: AttributeTag, value: Double): Unit = setDoubleList(al, tag, Seq(value))

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

  // private val beamRefList = Seq(1, 2, 3, 4, 5, 6, 7, 8) // TODO should get from RTPLAN
  val rtplanUid = "1.2.246.352.71.5.427549902257.1092044.20240207145418" // TODO should get from RTPLAN

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
    * @param al Image.
    * @param date New date.
    */
  private def setImageTimes(al: AttributeList, date: Date): Unit = {

    val pairList = Seq(
      (TagByName.AcquisitionDate, TagByName.AcquisitionTime),
      (TagByName.ContentDate, TagByName.ContentTime),
      (TagByName.InstanceCreationDate, TagByName.InstanceCreationTime)
    )

    pairList.foreach(pair => setTime(al, pair._1, pair._2, date))
  }

  /**
    * Set all of the date + time values for the given series.
    * @param alList Images for series sorted by time.
    * @param index Series number.
    */
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

  private def setImageAttr(al: AttributeList, SeriesInstanceUID: String, alIndex: Int, StudyInstanceUID: String, date: Date, machine: Machine, rtplan: AttributeList): Unit = {

    val rtplanBeam = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)(alIndex)

    val SOPInstanceUID = UMROGUID.getUID

    val InstitutionName = {
      val inst = Institution.get(machine.institutionPK).get
      inst.getRealName
    }

    val RadiationMachineName = {
      machine.getRealTpsId match {
        case Some(name) => name
        case _          => machine.getRealId
      }
    }

    val DeviceSerialNumber = machine.getRealDeviceSerialNumber.get

    val FrameOfReferenceUID = UMROGUID.getUID

    def setRTImagePosition: Unit = {

      val ImagePlanePixelSpacing = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues
      val Rows = al.get(TagByName.Rows).getIntegerValues.head
      val Columns = al.get(TagByName.Columns).getIntegerValues.head

      val x = ((1 - Columns) / 2.0) * ImagePlanePixelSpacing(0)
      val y = ((Rows - 1) / 2.0) * ImagePlanePixelSpacing(1)

      setDoubleList(al, TagByName.RTImagePosition, Seq(x, y)) // For TX4 this should be: -200.312, 150.136
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

    val PatientName = rtplan.get(TagByName.PatientName).getSingleStringValueOrEmptyString()
    val PatientID = rtplan.get(TagByName.PatientID).getSingleStringValueOrEmptyString()

    val ReferencedBeamNumber = rtplanBeam.get(TagByName.BeamNumber).getIntegerValues.head

    def setBeamLimitingDeviceAngle(): Unit = {
      val BeamLimitingDeviceAngle = DicomUtil.findAllSingle(rtplanBeam, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head
      setDouble(al, TagByName.BeamLimitingDeviceAngle, BeamLimitingDeviceAngle)
      val ExposureSequence = DicomUtil.seqToAttr(al, TagByName.ExposureSequence )
      ExposureSequence.foreach(ex => setDouble(ex, TagByName.BeamLimitingDeviceAngle, BeamLimitingDeviceAngle))
    }

    setText(al, TagByName.MediaStorageSOPInstanceUID, SOPInstanceUID)
    setText(al, TagByName.SpecificCharacterSet, "ISO_IR 192")
    setText(al, TagByName.SOPInstanceUID, SOPInstanceUID)
    setTime(al, TagByName.StudyDate, TagByName.StudyTime, date)
    setTime(al, TagByName.SeriesDate, TagByName.SeriesTime, date)
    setText(al, TagByName.InstitutionName, InstitutionName)
    setText(al, TagByName.StationName, RadiationMachineName)
    setText(al, TagByName.StudyDescription, "Machine QA")
    setText(al, TagByName.OperatorsName, "NA")

    setText(al, TagByName.ManufacturerModelName, "On-Board Imager")
    setText(al, TagByName.PatientName, PatientName)
    setText(al, TagByName.PatientID, PatientID)
    setText(al, TagByName.PatientBirthDate, "18000101")
    setText(al, TagByName.PatientBirthTime, "000000")
    setText(al, TagByName.PatientSex, "O")
    setText(al, TagByName.DeviceSerialNumber, DeviceSerialNumber)
    setText(al, TagByName.SoftwareVersions, "NA")
    setText(al, TagByName.PatientPosition, "HFS")
    setText(al, TagByName.StudyInstanceUID, StudyInstanceUID)
    setText(al, TagByName.SeriesInstanceUID, SeriesInstanceUID)
    setText(al, TagByName.StudyID, "NA")
    setInt(al, TagByName.SeriesNumber, 0)
    setText(al, TagByName.FrameOfReferenceUID, FrameOfReferenceUID)
    setText(al, TagByName.RTImageLabel, "NA")
    setText(al, TagByName.RTImageDescription, "NA")
    setIntList(al, TagByName.RTImageOrientation, Seq(1, 0, 0, 0, -1, 0))
    setText(al, TagByName.RadiationMachineName, RadiationMachineName)
    setDouble(al, TagByName.GantryAngle, 0) // TODO should get from RTPLAN
    al.put(ReferencedRTPlanSequence)
    setRTImagePosition
    setInt(al, TagByName.ReferencedBeamNumber, ReferencedBeamNumber)

    setBeamLimitingDeviceAngle()

  }

  private def processSeries(dir: File, index: Int, machine: Machine, rtplan: AttributeList): Unit = {
    Trace.trace(s"Reading directory ${dir.getAbsolutePath} ...")
    val fileList = Util.listDirFiles(dir).filter(_.getName.endsWith(".dcm")).filterNot(_.getName.endsWith("_NEW.dcm")).map(f => DicomFile(f))

    if (fileList.nonEmpty) {

      val alList = fileList.flatMap(_.attributeList).sortBy(al => getContentDateTime(al).getTime)

      if (true) { // TODO this is the way it should be
        val SeriesInstanceUID = UMROGUID.getUID
        val StudyInstanceUID = UMROGUID.getUID

        val seriesDate = setPerImageTime(alList, index)

        alList.indices.foreach(alIndex => setImageAttr(alList(alIndex), SeriesInstanceUID, alIndex, StudyInstanceUID, seriesDate, machine, rtplan))

        // alList.indices.foreach(index => setInt(alList(index), TagByName.ReferencedBeamNumber, beamRefList(index))) // TODO should get these from RTPLAN
      }

      val outDir = new File(dir, "out2")
      Util.deleteFileTreeSafely(outDir)
      outDir.mkdirs()

      Trace.trace(s"Saving files to directory ${outDir.getAbsolutePath}")

      val Modality = alList.head.get(TagByName.Modality).getSingleStringValueOrEmptyString()

      def writeDicom(index: Int): Unit = {
        val dcmFile = new File(outDir, Modality + (index + 1) + ".dcm")
        Util.writeAttributeListToFile(alList(index), dcmFile, "AQA")
        val txtFile = new File(outDir, Modality + (index + 1) + ".txt")
        Util.writeFile(txtFile, DicomUtil.attributeListToString(alList(index)))
      }

      alList.indices.foreach(writeDicom)
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
