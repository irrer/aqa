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

package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.Util

class DicomCsv {

  // abbreviate type for brevity
  private type AL = AttributeList

  val colList: Seq[CsvCol[AL]] = Seq(
    CsvCol("Pixel Rows", "Number of rows in the images.  DICOM 0028,0010", (al: AL) => Util.attr2Csv(al, TagByName.Rows).head),
    CsvCol("Pixel Columns", "Number of columns in the images. DICOM 0028,0011", (al: AL) => Util.attr2Csv(al, TagByName.Columns).head),
    CsvCol("Pixel size X", "Image Plane Pixel Spacing.  DICOM 3002,0011", (al: AL) => Util.attr2Csv(al, TagByName.ImagePlanePixelSpacing).head),
    CsvCol("Pixel size Y", "Image Plane Pixel Spacing.  DICOM 3002,0011", (al: AL) => Util.attr2Csv(al, TagByName.ImagePlanePixelSpacing)(1)),
    CsvCol("b value for converting pixel values to CU: CU = m*PixelValue + b", "Rescale Intercept.  DICOM 0028,1052", (al: AL) => Util.attr2Csv(al, TagByName.RescaleIntercept)(1)),
    CsvCol("m value for converting pixel values to CU: CU = m*PixelValue + b", "Rescale Slope.  DICOM 0028,1053", (al: AL) => Util.attr2Csv(al, TagByName.RescaleSlope)(1)),
    CsvCol("KVP", "Peak kilo voltage output of the X-Ray generator used.  DICOM 0018,0060", (al: AL) => Util.attr2Csv(al, TagByName.KVP).head),
    //noinspection SpellCheckingInspection
    CsvCol("Exposure Time msec", "Exposure time in mseync.  DICOM 0016,0004", (al: AL) => Util.attr2Csv(al, TagByName.ExposureTime).head),
    CsvCol(
      "Meterset Exposure",
      "Treatment machine Meterset duration over which image has been acquired, specified in Monitor units (MU) or minutes as defined by Primary Dosimeter Unit (300A,00B3). Required if Value 3 of Image Type (0008,0008) is PORTAL  DICOM 3002,0032",
      (al: AL) => Util.attr2Csv(al, TagByName.MetersetExposure).head
    ),
    CsvCol(
      "Start Cumulative Meterset Wt",
      "Cumulative Meterset Weight within Beam referenced by Referenced Beam Number (300C,0006) at which image acquisition starts.  DICOM 300C,0008",
      (al: AL) => Util.attr2Csv(al, TagByName.StartCumulativeMetersetWeight).head
    ),
    CsvCol(
      "X-Ray Origin X",
      "Position in (x,y,z) coordinates of origin of IEC X-RAY IMAGE RECEPTOR System in the IEC GANTRY coordinate system (mm). See Note 2.  DICOM 3002,000D",
      (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation).head
    ),
    CsvCol(
      "X-Ray Origin Y",
      "Position in (x,y,z) coordinates of origin of IEC X-RAY IMAGE RECEPTOR System in the IEC GANTRY coordinate system (mm). See Note 2.  DICOM 3002,000D",
      (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation)(1)
    ),
    CsvCol(
      "X-Ray Origin Z",
      "Position in (x,y,z) coordinates of origin of IEC X-RAY IMAGE RECEPTOR System in the IEC GANTRY coordinate system (mm). See Note 2.  DICOM 3002,000D",
      (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation)(2)
    ),
    CsvCol(
      "Source to Image",
      "Distance from radiation machine source to image plane (in mm) along radiation beam axis.  DICOM 3002,0026",
      (al: AL) => Util.attr2Csv(al, TagByName.RTImageSID).head
    ),
    CsvCol(
      "Source to Isocenter",
      "Radiation source to Gantry rotation axis distance of radiation machine used in acquiring or computing image (mm).  DICOM 3002,0022",
      (al: AL) => Util.attr2Csv(al, TagByName.RadiationMachineSAD).head
    ),
    CsvCol(
      "Gantry Angle",
      "Treatment machine gantry angle, i.e., orientation of IEC GANTRY coordinate system with respect to IEC FIXED REFERENCE coordinate system (degrees).  DICOM 300A,011E",
      (al: AL) => Util.attr2Csv(al, TagByName.GantryAngle).head
    ),
    CsvCol(
      "Collimator Angle",
      "Treatment machine beam limiting device (collimator) angle, i.e., orientation of IEC BEAM LIMITING DEVICE coordinate system with respect to IEC GANTRY coordinate system (degrees).  DICOM 300A,0120",
      (al: AL) => Util.attr2Csv(al, TagByName.BeamLimitingDeviceAngle).head
    ),
    CsvCol(
      "End Cumulative Meterset Wt",
      "Cumulative Meterset Weight within Beam referenced by Referenced Beam Number (300C,0006) at which image acquisition ends.  DICOM 300C,0009",
      (al: AL) => Util.attr2Csv(al, TagByName.EndCumulativeMetersetWeight).head
    ),
    CsvCol(
      "Beam Number",
      "Uniquely identifies the corresponding N-segment treatment beam specified by Beam Number (300A,00C0) within Beam Sequence (300A,00B0) in RT Beams Module within the RT Plan referenced in Referenced RT Plan Sequence (300C,0002) or the Ion Beam Sequence (300A,03A2) in the RT Ion Beams Module within the RT Ion Plan referenced in Referenced RT Plan Sequence (300C,0002).  DICOM 300C,0006",
      (al: AL) => Util.attr2Csv(al, TagByName.ReferencedBeamNumber).head
    ),
    CsvCol("Operator", "Anonymized name(s) of the operator(s) supporting the Series.  DICOM 0008,1070", (al: AL) => Util.attr2Csv(al, TagByName.OperatorsName).head),
    CsvCol(
      "Software Version",
      "Manufacturer's designation of software version of the equipment that produced the sources.  DICOM 0018,1020",
      (al: AL) => Util.attr2Csv(al, TagByName.SoftwareVersions).mkString("  ")
    ),
    CsvCol("SOPInstanceUID", "Uniquely identifies the SOP Instance.  DICOM 0008,0018", (al: AL) => Util.attr2Csv(al, TagByName.SOPInstanceUID).head),
    CsvCol(
      "SeriesInstanceUID",
      "Unique identifier for the Series that is part of the Study identified in Study Instance UID (0020,000D), if present, and contains the referenced object instance(s).  DICOM 0020,000E",
      (al: AL) => Util.attr2Csv(al, TagByName.SeriesInstanceUID).head
    ),
    CsvCol("PatientID", "Primary identifier for the Patient.  DICOM 0010,0020", (al: AL) => Util.attr2Csv(al, TagByName.PatientID).head),
    CsvCol("PatientName", "Patient's full name DICOM 0010,0010", (al: AL) => Util.attr2Csv(al, TagByName.PatientName).head)
  )

  def dicomToText(al: AL): String = {
    colList.map(c => c.toText(al)).mkString(",")
  }

  /** The CSV headers for prefix. */
  def headerText(headerPrefix: Option[String] = None): String = {
    def toHeader(header: String) = {
      val text = {
        if (headerPrefix.isEmpty) header
        else headerPrefix.get + ": " + header
      }
      Util.textToCsv(text)
    }
    colList.map(c => toHeader(c.header)).mkString(",")
  }

}
