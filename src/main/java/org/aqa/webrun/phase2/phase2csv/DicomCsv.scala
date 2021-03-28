package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.Util

class DicomCsv {

  // abbreviate type for brevity
  private type AL = AttributeList

  val colList: Seq[CsvCol[AL]] = Seq(
    CsvCol("Pixel Rows", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.Rows).head),
    CsvCol("Pixel Columns", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.Columns).head),
    CsvCol("Pixel size X", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.ImagePlanePixelSpacing).head),
    CsvCol("Pixel size Y", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.ImagePlanePixelSpacing)(1)),
    CsvCol("KVP", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.KVP).head),
    CsvCol("Exposure Time msec", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.ExposureTime).head),
    CsvCol("Meterset Exposure", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.MetersetExposure).head),
    CsvCol("Start Cumulative Meterset Wt", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.StartCumulativeMetersetWeight).head),
    CsvCol("X-Ray Origin X", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation).head),
    CsvCol("X-Ray Origin Y", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation)(1)),
    CsvCol("X-Ray Origin Z", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation)(2)),
    CsvCol("Source to Image", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.RTImageSID).head),
    CsvCol("Source to Isocenter", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.RadiationMachineSAD).head),
    CsvCol("Gantry Angle", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.GantryAngle).head),
    CsvCol("Collimator Angle", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.BeamLimitingDeviceAngle).head),
    CsvCol("End Cumulative Meterset Wt", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.EndCumulativeMetersetWeight).head),
    CsvCol("Operator", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.OperatorsName).head),
    CsvCol("Software Version", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.SoftwareVersions).mkString("  ")),
    CsvCol("SOPInstanceUID", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.SOPInstanceUID).head),
    CsvCol("SeriesInstanceUID", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.SeriesInstanceUID).head),
    CsvCol("PatientID", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.PatientID).head),
    CsvCol("PatientName", "jjjjj", (al: AL) => Util.attr2Csv(al, TagByName.PatientName).head)
  )

  def dicomToText(al: AL): String = {
    colList.map(c => c.toText(al)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
