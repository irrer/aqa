package org.aqa

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.DicomFileUtilities

object ShowSerialNumber { // TODO should this be in production?

  private def showSerialNumber(file: File): Unit = {

    try {
      if (DicomFileUtilities.isDicomOrAcrNemaFile(file)) {
        val dicomFile = new DicomFile(file)
        if (dicomFile.attributeList.isDefined) {
          val al = dicomFile.attributeList.get
          def valOf(tag: AttributeTag): String = {
            val at = al.get(tag)
            if (at == null) ""
            else al.get(tag).getSingleStringValueOrEmptyString
          }

          val DeviceSerialNumber = valOf(TagFromName.DeviceSerialNumber)
          val StationName = valOf(TagFromName.StationName)
          val Modality = valOf(TagFromName.Modality)
          val SOPInstanceUID = valOf(TagFromName.SOPInstanceUID)
          val SeriesInstanceUID = valOf(TagFromName.SeriesInstanceUID)

          val AcquisitionDate = valOf(TagFromName.AcquisitionDate)
          val AcquisitionTime = valOf(TagFromName.AcquisitionTime)

          val ContentDate = valOf(TagFromName.ContentDate)
          val ContentTime = valOf(TagFromName.ContentTime)

          val InstanceCreationDate = valOf(TagFromName.InstanceCreationDate)
          val InstanceCreationTime = valOf(TagFromName.InstanceCreationTime)

          val date = if (InstanceCreationDate.nonEmpty) InstanceCreationDate else if (AcquisitionDate.nonEmpty) AcquisitionDate else ContentDate
          val time = if (InstanceCreationTime.nonEmpty) InstanceCreationTime else if (AcquisitionTime.nonEmpty) AcquisitionTime else ContentTime

          println(DeviceSerialNumber + " : " +
            StationName + " : " + Modality +
            "  SeriesInstanceUID: " + SeriesInstanceUID +
            " : " + "  SOPInstanceUID: " + SOPInstanceUID +
            " : " + "  : " + date + "::" + time +
            " : " + file.getAbsolutePath)
        }
      }
    } catch {
      case t: Throwable => ;
    }

    if (file.isDirectory) file.listFiles.map(f => showSerialNumber(f))
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    println("Starting...")
    val fileList = args.map(a => new File(a))
    fileList.map(file => showSerialNumber(file))
    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}