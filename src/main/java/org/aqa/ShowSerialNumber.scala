/*
 * Copyright 2021 Regents of the University of Michigan
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

package org.aqa

import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.TagFromName

import java.io.File

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

          println(
            DeviceSerialNumber + " : " +
              StationName + " : " + Modality +
              "  SeriesInstanceUID: " + SeriesInstanceUID +
              " : " + "  SOPInstanceUID: " + SOPInstanceUID +
              " : " + "  : " + date + "::" + time +
              " : " + file.getAbsolutePath
          )
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
