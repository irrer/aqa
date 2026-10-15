import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.UMROGUID
import org.aqa.DicomFile
import org.aqa.Util

import java.io.File
import java.util.Date

/*
 * Copyright 2026 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

object MakeMinimalStakitt {

  private val dir = new File("""D:\tmp\stakittDicom\phase3\required""")

  private val newStudyInstanceUID = UMROGUID.getUID
  private val newSeriesInstanceUID = UMROGUID.getUID
  private val newRtplanSOPUID = UMROGUID.getUID
  private val newFrameOfReferenceUID = UMROGUID.getUID
  private val newDoseReferenceUID = UMROGUID.getUID

  private val newDateTime = new Date()

  private val oneMinute_ms = 60 * 1000

  private val newDate = DicomUtil.dicomDateFormat.format(newDateTime)
  private val newTimeRtplan = DicomUtil.dicomTimeFormat.format(newDateTime)
  private val newTimeC090 = DicomUtil.dicomTimeFormat.format(new Date(newDateTime.getTime + (oneMinute_ms * 1)))
  private val newTimeC270 = DicomUtil.dicomTimeFormat.format(new Date(newDateTime.getTime + (oneMinute_ms * 2)))
  private val newTimeStakitt = DicomUtil.dicomTimeFormat.format(new Date(newDateTime.getTime + (oneMinute_ms * 3)))

  private def getDicomFile(name: String): AttributeList = {
    new DicomFile(new File(dir, name)).al
  }

  private val rtplan = getDicomFile("rtplan.dcm")
  private val aqaRtplan = getDicomFile("aqaRtplan.dcm")
  private val stakitt = getDicomFile("stakitt.dcm")
  private val c090 = getDicomFile("c090.dcm")
  private val c270 = getDicomFile("c270.dcm")

  private val c090BeamNumber = 24
  private val c270BeamNumber = 23

  private val aqaC090BeamNumber = 2
  private val aqaC270BeamNumber = 18

  private val c090Beam = Util.getBeamOfRtimage(rtplan, c090BeamNumber).get
  private val c270Beam = Util.getBeamOfRtimage(rtplan, c270BeamNumber).get

  private val DeviceSerialNumber = DicomUtil.findAllTag(rtplan, TagByName.DeviceSerialNumber).head.getSingleStringValueOrEmptyString()

  private def setAttr(al: AttributeList, tag: AttributeTag, text: String): Unit = {
    def setValue(attr: Attribute): Unit = {
      attr.removeValues()
      attr.addValue(text)
    }
    DicomUtil.findAllTag(al, tag).foreach(setValue)
  }

  private val aqaCollimatorCenteringBeam090 = DicomUtil.getBeamOfRtimage(aqaRtplan, aqaC090BeamNumber).get
  private val aqaCollimatorCenteringBeam270 = DicomUtil.getBeamOfRtimage(aqaRtplan, aqaC270BeamNumber).get

  private def setRtplanAttributes(): Unit = {

    // try copying all attributes
    if (true) {
      import scala.jdk.CollectionConverters._
      c090Beam.clear()
      val tagList090 = aqaCollimatorCenteringBeam090.keySet().asScala
      tagList090.map(tag => c090Beam.put(aqaCollimatorCenteringBeam090.get(tag)))
      setAttr(c090Beam, TagByName.BeamName, "M10G0C90")

      c270Beam.clear()
      val tagList270 = aqaCollimatorCenteringBeam270.keySet().asScala
      tagList270.map(tag => c270Beam.put(aqaCollimatorCenteringBeam270.get(tag)))
      setAttr(c270Beam, TagByName.BeamName, "M10G0C270")
    }

    if (true) {
      val attrList = DicomUtil.findAllTag(rtplan, TagByName.ReferencedBeamNumber)
      attrList.foreach(attr => {
        val v = attr.getIntegerValues.head

        if (v == c090BeamNumber) {
          attr.removeValues()
          attr.addValue(aqaC090BeamNumber)
        }

        if (v == c270BeamNumber) {
          attr.removeValues()
          attr.addValue(aqaC270BeamNumber)
        }

      })
    }

    def setAt(tag: AttributeTag, text: String): Unit = setAttr(rtplan, tag, text)

    setAt(TagByName.StudyInstanceUID, newStudyInstanceUID)

    setAt(TagByName.MediaStorageSOPInstanceUID, newRtplanSOPUID)

    setAt(TagByName.SOPInstanceUID, newRtplanSOPUID)

    setAt(TagByName.FrameOfReferenceUID, newFrameOfReferenceUID)

    setAt(TagByName.DoseReferenceUID, newDoseReferenceUID)

    setAt(TagByName.InstanceCreationDate, newDate)
    setAt(TagByName.StudyDate, newDate)
    setAt(TagByName.RTPlanDate, newDate)

  }

  private def setCenteringRtimageAttributes(): Unit = {
    def setBothAt(tag: AttributeTag, text: String): Unit = {
      setAttr(c090, tag, text)
      setAttr(c270, tag, text)
    }

    setBothAt(TagByName.ReferencedSOPInstanceUID, newRtplanSOPUID)

    // setAttr(c090, TagByName.ReferencedBeamNumber, c090BeamNumber.toString)
    // setAttr(c270, TagByName.ReferencedBeamNumber, c270BeamNumber.toString)

    val c090UID = UMROGUID.UMRO_ROOT_GUID
    setAttr(c090, TagByName.MediaStorageSOPInstanceUID, c090UID)
    setAttr(c090, TagByName.SOPInstanceUID, c090UID)

    val c270UID = UMROGUID.UMRO_ROOT_GUID
    setAttr(c270, TagByName.MediaStorageSOPInstanceUID, c270UID)
    setAttr(c270, TagByName.SOPInstanceUID, c270UID)

    setBothAt(TagByName.StudyInstanceUID, newStudyInstanceUID)

    setBothAt(TagByName.SeriesInstanceUID, newSeriesInstanceUID)

    setBothAt(TagByName.DeviceSerialNumber, DeviceSerialNumber)

    setBothAt(TagByName.FrameOfReferenceUID, newFrameOfReferenceUID)

    setBothAt(TagByName.InstanceCreationDate, newDate)
    setBothAt(TagByName.StudyDate, newDate)
    setBothAt(TagByName.AcquisitionDate, newDate)
    setBothAt(TagByName.ContentDate, newDate)

    setBothAt(TagByName.StudyTime, newTimeRtplan)

    setAttr(c090, TagByName.InstanceCreationTime, newTimeC090)
    setAttr(c090, TagByName.AcquisitionTime, newTimeC090)
    setAttr(c090, TagByName.ContentTime, newTimeC090)

    setAttr(c270, TagByName.InstanceCreationTime, newTimeC270)
    setAttr(c270, TagByName.AcquisitionTime, newTimeC270)
    setAttr(c270, TagByName.ContentTime, newTimeC270)
  }

  private def setStakittRtimageAttributes(): Unit = {

    def setAt(tag: AttributeTag, text: String): Unit = setAttr(stakitt, tag, text)

    val date = c090.get(TagByName.AcquisitionDate).getSingleStringValueOrEmptyString()

    setAt(TagByName.AcquisitionDate, date)
    setAt(TagByName.InstanceCreationDate, date)
    setAt(TagByName.ContentDate, date)

    setAt(TagByName.ReferencedSOPInstanceUID, newRtplanSOPUID)

    setAt(TagByName.DeviceSerialNumber, DeviceSerialNumber)

    setAt(TagByName.SeriesInstanceUID, newSeriesInstanceUID)

    setAt(TagByName.StudyInstanceUID, newStudyInstanceUID)

    setAt(TagByName.FrameOfReferenceUID, newFrameOfReferenceUID)

    setAt(TagByName.StudyTime, newTimeRtplan)
    setAt(TagByName.SeriesTime, newTimeRtplan)
    setAt(TagByName.InstanceCreationTime, newTimeStakitt)
    setAt(TagByName.AcquisitionTime, newTimeStakitt)
    setAt(TagByName.ContentTime, newTimeStakitt)
  }

  private def writeDicom(): Unit = {
    val outDir = new File(dir, "OUT_" + Util.currentTimeAsFileName)
    outDir.mkdirs()

    DicomUtil.writeAttributeListToFile(rtplan, new File(outDir, "rtplan.dcm"), "AQA")
    DicomUtil.writeAttributeListToFile(stakitt, new File(outDir, "stakitt.dcm"), "AQA")
    DicomUtil.writeAttributeListToFile(c090, new File(outDir, "c090.dcm"), "AQA")
    DicomUtil.writeAttributeListToFile(c270, new File(outDir, "c270.dcm"), "AQA")

    println(s"DICOM written to ${outDir.getAbsolutePath}")
  }

  def main(args: Array[String]): Unit = {
    println("starting")

    setRtplanAttributes()

    setCenteringRtimageAttributes()

    setStakittRtimageAttributes()

    writeDicom()

    println("finished")
  }

}
