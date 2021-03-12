package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.Util

class DicomCsv {


  // abbreviate type for brevity
  private type AL = AttributeList

  private case class DicomCol(header: String, toText: AL => String) {}

  private val colList = Seq(
    DicomCol("Pixel Rows", (al: AL) => Util.attr2Csv(al, TagByName.Rows).head),
    DicomCol("Pixel Columns", (al: AL) => Util.attr2Csv(al, TagByName.Columns).head),
    DicomCol("Pixel size X", (al: AL) => Util.attr2Csv(al, TagByName.ImagePlanePixelSpacing).head),
    DicomCol("Pixel size Y", (al: AL) => Util.attr2Csv(al, TagByName.ImagePlanePixelSpacing)(1)),
    DicomCol("KVP", (al: AL) => Util.attr2Csv(al, TagByName.KVP).head),
    DicomCol("Exposure Time msec", (al: AL) => Util.attr2Csv(al, TagByName.ExposureTime).head),
    DicomCol("Meterset Exposure", (al: AL) => Util.attr2Csv(al, TagByName.MetersetExposure).head),
    DicomCol("Start Cumulative Meterset Wt", (al: AL) => Util.attr2Csv(al, TagByName.StartCumulativeMetersetWeight).head),
    DicomCol("X-Ray Origin X", (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation).head),
    DicomCol("X-Ray Origin Y", (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation)(1)),
    DicomCol("X-Ray Origin Z", (al: AL) => Util.attr2Csv(al, TagByName.XRayImageReceptorTranslation)(2)),
    DicomCol("Source to Image", (al: AL) => Util.attr2Csv(al, TagByName.RTImageSID).head),
    DicomCol("Source to Isocenter", (al: AL) => Util.attr2Csv(al, TagByName.RadiationMachineSAD).head),
    DicomCol("Gantry Angle", (al: AL) => Util.attr2Csv(al, TagByName.GantryAngle).head),
    DicomCol("Collimator Angle", (al: AL) => Util.attr2Csv(al, TagByName.BeamLimitingDeviceAngle).head),
    DicomCol("End Cumulative Meterset Wt", (al: AL) => Util.attr2Csv(al, TagByName.EndCumulativeMetersetWeight).head),
    DicomCol("Operator", (al: AL) => Util.attr2Csv(al, TagByName.OperatorsName).head),
    DicomCol("Software Version", (al: AL) => Util.attr2Csv(al, TagByName.SoftwareVersions).mkString("  ")),
    DicomCol("PatientID", (al: AL) => Util.attr2Csv(al, TagByName.PatientID).head),
    DicomCol("PatientName", (al: AL) => Util.attr2Csv(al, TagByName.PatientName).head),
  )

  def dicomToText(al: AL): String = {
    colList.map(c => c.toText(al)).mkString(",")
  }

  /** The CSV headers for prefix. */
  val headerText: String = colList.map(c => c.header).mkString(",")

}
