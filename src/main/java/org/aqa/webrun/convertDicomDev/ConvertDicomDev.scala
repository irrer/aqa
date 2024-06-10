package org.aqa.webrun.convertDicomDev

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Machine
import org.aqa.Logging
import org.aqa.Util

import java.io.File

case object ConvertDicomDev extends Logging {

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

  private def setDouble(al: AttributeList, tag: AttributeTag, value: Int): Unit = setIntList(al, tag, Seq(value))

  private def getRtPlanUid(rtimageList: Seq[AttributeList], rtplan: Option[AttributeList]): String = {
    if (rtplan.isDefined)
      Util.sopOfAl(rtplan.get)
    else {
      val j = rtimageList.flatMap(Util.getRtplanSop).groupBy(uid => uid).maxBy(_._2.length)._1
      ???

    }
  }

  private def editGeneralImage(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {

    setText(newRtimage, TagByName.SpecificCharacterSet, "SpecificCharacterSet")
    setTextList(newRtimage, TagByName.ImageType, Seq("ORIGINAL" , "PRIMARY" , "PORTAL" , "ACQUIRED_DOSE"))
    // setText(newRtimage, TagByName.SOPClassUID: 1.2.840.10008.5.1.4.1.1.481.1 (RT Image Storage)
    /*
    0008,0005 CS  SpecificCharacterSet: ISO_IR 192
    0008,0008 CS  ImageType: ORIGINAL \ PRIMARY \ PORTAL \ ACQUIRED_DOSE

    0008,0005 CS  SpecificCharacterSet: ISO_IR 192
    0008,0016 UI  SOPClassUID: 1.2.840.10008.5.1.4.1.1.481.1 (RT Image Storage)
    0008,0018 UI  SOPInstanceUID: 1.3.6.1.4.1.9590.100.1.2.315781879413008622216173623132623997702
    0008,0020 DA  StudyDate: 20240208
    0008,0021 DA  SeriesDate: 20240212
    0008,0022 DA  AcquisitionDate: 20240212
    0008,0023 DA  ContentDate: 20240212
    0008,0030 TM  StudyTime: 174313.927
    0008,0031 TM  SeriesTime: 113618.17
    0008,0032 TM  AcquisitionTime: 112644
    0008,0033 TM  ContentTime: 112644
    0008,0050 SH  AccessionNumber: none
    0008,0060 CS  Modality: RTIMAGE
    0008,0064 CS  ConversionType: DI
    0008,0070 LO  Manufacturer: Varian Medical Systems
    0008,0080 LO  InstitutionName: IT Test
    0008,0090 PN  ReferringPhysicianName: none
     */
  }

  private def editGeneralEquipment(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*

0008,1010 SH  StationName: UM-EX4
0008,1030 LO  StudyDescription: ARIA RadOnc Study
0008,1070 PN  OperatorsName: DICOM Service
0008,1090 LO  ManufacturerModelName: On-Board Imager
     */
  }

  private def editPatient(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
0010,0010 PN  PatientName: $AQA_TX4
0010,0020 LO  PatientID: $AQA_TX4
0010,0030 DA  PatientBirthDate: 18000101
0010,0032 TM  PatientBirthTime: 000000
0010,0040 CS  PatientSex: O

     */
  }

  private def editDevice(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
0018,1000 LO  DeviceSerialNumber: 3340
0018,1020 LO  SoftwareVersions: 1.6.20.0
0018,5100 CS  PatientPosition: HFS
     */
  }

  private def editSOPCommon(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
0020,000d UI  StudyInstanceUID: 1.3.6.1.4.1.22361.2269929987485.243413907.1707404140384.15
0020,000e UI  SeriesInstanceUID: 1.2.246.352.61.2.5011036419594195404.8858886508374365351
0020,0010 SH  StudyID: none12
0020,0011 IS  SeriesNumber: 0
0020,0013 IS  InstanceNumber: <null>
0020,0020 CS  PatientOrientation: <null>
0020,0052 UI  FrameOfReferenceUID: 1.2.246.352.61.8.5561830440337236413.11097381583245055616
0020,1040 LO  PositionReferenceIndicator: <null>
     */
  }

  private def editImage(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
0028,0002 US  SamplesPerPixel: 1
0028,0004 CS  PhotometricInterpretation: MONOCHROME2
0028,0010 US  Rows: 384
0028,0011 US  Columns: 512
0028,0100 US  BitsAllocated: 16
0028,0101 US  BitsStored: 16
0028,0102 US  HighBit: 15
0028,0103 US  PixelRepresentation: 0
0028,0106 XS  SmallestImagePixelValue: 11158
0028,0107 XS  LargestImagePixelValue: 16384
0028,1050 DS  WindowCenter: 0.50439940096
0028,1051 DS  WindowWidth: 0.99861272576
0028,1052 DS  RescaleIntercept: 3.1286864974821
0028,1053 DS  RescaleSlope: -0.00019095987
0028,1054 LO  RescaleType: CU
     */
  }

  private def editRTImage(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
3002,0002 SH  RTImageLabel: 01:Flood 6X-15_1
3002,0004 ST  RTImageDescription: 6 MV, 600 MU/min   Scan Mode IDU20-Half-LoX-Integrated   Averaged Frames 241   Frame Statistics 1, 241, 241, 0, 0, 0, 0, 0, 0, 0   Dark Field Correction on  Flood Field Correction on  Static Pixel Defects Correction on  Random Pixel Defects Correction off
3002,000a CS  ReportedValuesOrigin: ACTUAL
3002,000c CS  RTImagePlane: NORMAL
3002,000d DS  XRayImageReceptorTranslation: -0.5851889380954 \ 0.9793146681622 \ -500
3002,000e DS  XRayImageReceptorAngle: 0
3002,0010 DS  RTImageOrientation: 1 \ 0 \ 0 \ 0 \ -1 \ 0
3002,0011 DS  ImagePlanePixelSpacing: 0.784 \ 0.784
3002,0012 DS  RTImagePosition: -200.312 \ 150.136
3002,0020 SH  RadiationMachineName: UM-EX4
3002,0022 DS  RadiationMachineSAD: 1000
3002,0026 DS  RTImageSID: 1500
3002,0029 IS  FractionNumber: 1
3002,0030 SQ  ExposureSequence:
  Item 1 / 1
    0008,1160 IS  ReferencedFrameNumber: 1
    0018,0060 DS  KVP: 6000
    3002,0032 DS  MetersetExposure: 200.06
    300a,00b6 SQ  BeamLimitingDeviceSequence:
      Item 1 / 2
        300a,00b8 CS  RTBeamLimitingDeviceType: X
        300a,00bc IS  NumberOfLeafJawPairs: 1
        300a,011c DS  LeafJawPositions: -120 \ 120
      Item 2 / 2
        300a,00b8 CS  RTBeamLimitingDeviceType: Y
        300a,00bc IS  NumberOfLeafJawPairs: 1
        300a,011c DS  LeafJawPositions: -90 \ 90
    300a,00f0 IS  NumberOfBlocks: 0
    300a,011e DS  GantryAngle: 0
    300a,0120 DS  BeamLimitingDeviceAngle: 0
    300a,0122 DS  PatientSupportAngle: 0
    300a,0128 DS  TableTopVerticalPosition: -150
    300a,0129 DS  TableTopLongitudinalPosition: 251
    300a,012a DS  TableTopLateralPosition: -9
     */
  }

  private def editRTImageSetup(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
300a,00b3 CS  PrimaryDosimeterUnit: MU
300a,011e DS  GantryAngle: 0
300a,0120 DS  BeamLimitingDeviceAngle: 0
300a,0122 DS  PatientSupportAngle: 0
300a,0128 DS  TableTopVerticalPosition: -150
300a,0129 DS  TableTopLongitudinalPosition: 251
300a,012a DS  TableTopLateralPosition: -9
300a,012c DS  IsocenterPosition: 9 \ -150 \ -251
     */
  }

  private def editRTImageReferences(oldRtimage: AttributeList, newRtimage: AttributeList): Unit = {
    /*
300c,0002 SQ  ReferencedRTPlanSequence:
  Item 1 / 1
    0008,1150 UI  ReferencedSOPClassUID: 1.2.840.10008.5.1.4.1.1.481.5 (RT Plan Storage)
    0008,1155 UI  ReferencedSOPInstanceUID: 1.2.246.352.71.5.427549902257.1092564.20240208125522
300c,0006 IS  ReferencedBeamNumber: 15
300c,0022 IS  ReferencedFractionGroupNumber: 1
     */
  }

  private def convertRtImage(oldRtimage: AttributeList, rtplan: AttributeList, machine: Machine): AttributeList = {
    val newRtimage = DicomUtil.clone(oldRtimage)

    /*
---------------------------------------------------------------------------------------------------
0002,0000 UL  FileMetaInformationGroupLength: 212
0002,0001 OB  FileMetaInformationVersion: 0x0 0x1
0002,0002 UI  MediaStorageSOPClassUID: 1.2.840.10008.5.1.4.1.1.481.1 (RT Image Storage)
0002,0003 UI  MediaStorageSOPInstanceUID: 1.3.6.1.4.1.9590.100.1.2.315781879413008622216173623132623997702
0002,0010 UI  TransferSyntaxUID: 1.2.840.10008.1.2
0002,0012 UI  ImplementationClassUID: 1.3.6.1.4.1.9590.100.1.3.100.9.4
0002,0013 SH  ImplementationVersionName: MATLAB IPT 9.4
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 ---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
7fe0,0010 OX  PixelData: 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3f 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3e 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d 0x3d
---------------------------------------------------------------------------------------------------

    val SOPInstanceUID = UMROGUID.getUID

    setSpecificCharacterSet
    setStudyDate
    setSeriesDate
    setAcquisitionDate
    setContentDate
    setStudyTime
    setSeriesTime
    setAcquisitionTime
    setContentTime
    setInstitutionName
    setStationName
    setStudyDescription: ARIA RadOnc Study
    setOperatorsName: DICOM Service
    setManufacturerModelName: On-Board Imager
    setPatientName: $AQA_TX4
    setPatientID: $AQA_TX4
    setPatientBirthDate: 18000101
    setPatientBirthTime: 000000
    setPatientSex: O
    setDeviceSerialNumber: 3340
    setPatientPosition: HFS
    setFrameOfReferenceUID
       setIntList(newRtimage, TagByName.RTImageOrientation, Seq(1 , 0 , 0 , 0 , -1 , 0)) // RTImageOrientation: 1 , 0 , 0 , 0 , -1 , 0
    setRTImagePosition
    setRadiationMachineName
    setReferencedRTPlanSequence

    setReferencedBeamNumber *********
     */

    ???
  }

  def convertRtImageList(rtimageList: Seq[AttributeList], rtplan: Option[AttributeList], machine: Machine): Seq[AttributeList] = {
    val StudyInstanceUID = UMROGUID.getUID
    val SeriesInstanceUID = UMROGUID.getUID

    val planUID = getRtPlanUid(rtimageList, rtplan)

    // rtimageList.map()
    ???
  }

  private def process(dir: File, index: Int): Unit = {
    val fileList = Util.listDirFiles(dir).filter(_.getName.endsWith("_NEW.dcm"))
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("Starting")

    val dirList = Util.listDirFiles( new File("""D:\tmp\FocalSpot_TX4\symmetry""")).filter(_.isDirectory).sortBy(_.getName)

    dirList.indices.foreach(i => process(dirList(i), i))



    Trace.trace("Done")
  }
}
