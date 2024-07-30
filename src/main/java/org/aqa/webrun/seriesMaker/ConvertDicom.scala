package org.aqa.webrun.seriesMaker

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.SOPClass
import edu.umro.util.UMROGUID
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.webrun.seriesMaker.SeriesMakerReq.getContentDateTime

import java.util.Date

object ConvertDicom extends Logging {

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

  // private def setText(al: AttributeList, tag: AttributeTag, value: Option[String]): Unit = if (value.isDefined) setTextList(al, tag, Seq(value.get))

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

  /**
   * Given the new beam captured in development mode, and a normal template beam, create a
   * new beam with the pixel of the dev beam, and with most of the metadata of the template
   * beam, but with new UIDs and date/times.
   *
   * @param dev                  Beam delivered in development mode.
   * @param template             Beam delivered in normal production mode.
   * @param SeriesInstanceUID    Series UID to be used for series.
   * @param StudyInstanceUID     Study UID to be used for series.
   * @param instanceDate         Date/time for this instance.
   * @param seriesDate           Date/time for the series.
   * @param studyDate            Date/time for the study.
   * @param RadiationMachineName Machine name
   * @param DeviceSerialNumber   Device serial number
   * @param PatientName          Patient name
   * @param PatientID            Patient ID
   * @param InstitutionName      String,
   * @param rtplanUid            Point to this RTPLAN UID.
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
                            RadiationMachineName: String,
                            DeviceSerialNumber: String,
                            PatientName: String,
                            PatientID: String,
                            InstitutionName : String,
                            rtplanUid: String)
  // @formatter:on
  : AttributeList = {

    val newAl = DicomUtil.clone(template)

    // ascertain values needed for new file

    // val rtplanBeam = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)(index)

    val SOPInstanceUID = UMROGUID.getUID


    val FrameOfReferenceUID = UMROGUID.getUID

    val RTImagePosition: Seq[Double] = {

      val ImagePlanePixelSpacing = dev.get(TagByName.ImagePlanePixelSpacing).getDoubleValues
      val Rows = dev.get(TagByName.Rows).getIntegerValues.head
      val Columns = dev.get(TagByName.Columns).getIntegerValues.head

      val x = ((1 - Columns) / 2.0) * ImagePlanePixelSpacing(0)
      val y = ((Rows - 1) / 2.0) * ImagePlanePixelSpacing(1)

      Seq(x, y)
    }

    val ReferencedRTPlanSequence: SequenceAttribute = {
      val rps = AttributeFactory.newAttribute(TagByName.ReferencedRTPlanSequence).asInstanceOf[SequenceAttribute]
      val subAl = new AttributeList
      setText(subAl, TagByName.ReferencedSOPClassUID, SOPClass.RTPlanStorage)
      setText(subAl, TagByName.ReferencedSOPInstanceUID, rtplanUid)
      rps.addItem(subAl)
      rps
    }

    val ContentTime = getContentDateTime(dev)

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
    setText(newAl, TagByName.PatientName, PatientName)
    setText(newAl, TagByName.PatientID, PatientID)
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

    val metaFromDev: Seq[AttributeTag] = Seq(
      TagByName.ImageComments,
      TagByName.SamplesPerPixel,
      TagByName.PhotometricInterpretation,
      TagByName.Rows,
      TagByName.Columns,
      TagByName.BitsAllocated,
      TagByName.BitsStored,
      TagByName.HighBit,
      TagByName.PixelRepresentation,
      TagByName.WindowCenter,
      TagByName.WindowWidth,
      TagByName.RescaleIntercept,
      TagByName.RescaleSlope,
      TagByName.RescaleType,
      TagByName.RTImageLabel,
      TagByName.RTImageName,
      TagByName.RTImageDescription,
      TagByName.ReportedValuesOrigin,
      TagByName.RTImagePlane,
      TagByName.XRayImageReceptorTranslation,
      TagByName.XRayImageReceptorAngle,
      TagByName.ImagePlanePixelSpacing,
      TagByName.RadiationMachineSAD,
      TagByName.RTImageSID)

    def copyFromDev(tag: AttributeTag): Unit = {
      val attr = dev.get(tag)
      if ((attr != null) && (attr.getSingleStringValueOrNull != null))
        newAl.put(attr)
    }

    metaFromDev.foreach(copyFromDev)

    // setText(newAl, TagByName.RTImageLabel, "NA")
    // setText(newAl, TagByName.RTImageDescription, "NA")
    // setIntList(newAl, TagByName.RTImageOrientation, Seq(1, 0, 0, 0, -1, 0))
    setText(newAl, TagByName.RadiationMachineName, RadiationMachineName)
    // setDouble(newAl, TagByName.GantryAngle, 0)
    newAl.put(ReferencedRTPlanSequence)
    setDouble(newAl, TagByName.RTImagePosition, RTImagePosition)
    // setInt(newAl, TagByName.ReferencedBeamNumber, ReferencedBeamNumber)

    newAl.remove(TagByName.PixelData)
    newAl.put(dev.get(TagByName.PixelData))

    newAl
  }

  def processSeries(beamConversionList: Seq[BeamConversion],
                    RadiationMachineName: String,
                    DeviceSerialNumber: String,
                    PatientName: String,
                    PatientID: String,
                    InstitutionName: String,
                    rtplanUid: String
                   ): Seq[AttributeList] = {

    val devList = beamConversionList.map(_.dev)
    val templateList = beamConversionList.map(_.template)

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
        RadiationMachineName: String,
        DeviceSerialNumber: String,
        PatientName = PatientName,
        PatientID = PatientID,
        InstitutionName: String,
        rtplanUid = rtplanUid
      ))

    newAlList

  }

}
