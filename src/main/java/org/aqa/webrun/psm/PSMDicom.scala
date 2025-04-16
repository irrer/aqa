package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.UMROGUID
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Trace

/**
  * Provide functions to convert a PSM image to and from DICOM.
  *
  * Note that a small degree of precision is lost because the pixels are stored as 16-bit integers.
  * When a PSM analysis is done, a round-trip to DICOM and back is performed, and the largest errors are logged.
  * In testing, the largest error was  2.0861626E-6 == 0.0000020861626   .
  */
object PSMDicom {

  /**
    * Make DICOM out of PSM image.
    *
    * This DICOM will have a new SOPInstanceUID and SeriesInstanceUID.  It uses 16-bit pixels.
    *
    * @param dicomImage PSM values.
    * @param prototype One of the original PSM images.
    * @return
    */
  def psmToDicom(dicomImage: DicomImage, prototype: AttributeList, RTImageLabel: String, RTImageDescription: String): AttributeList = {

    val al = DicomUtil.clone(prototype)

    def set(tag: AttributeTag, value: Any): Unit = {
      val attr = AttributeFactory.newAttribute(tag)
      attr.removeValues()

      value match {
        case str: String => attr.addValue(str)
        case dbl: Double => attr.addValue(dbl)
        case flt: Float  => attr.addValue(flt)
        case int: Int    => attr.addValue(int)
      }

      al.put(attr)
    }

    val sopUid = UMROGUID.getUID
    set(TagByName.MediaStorageSOPInstanceUID, sopUid)
    set(TagByName.SOPInstanceUID, sopUid)
    set(TagByName.SeriesInstanceUID, UMROGUID.getUID)
    set(TagByName.RTImageLabel, RTImageLabel)
    set(TagByName.RTImageDescription, RTImageDescription)
    set(TagByName.ReferencedBeamNumber, -1)

    val hiPix = 0xffff

    if (true) { // TODO Exploring the level of precision. rm
      /*
      Example numbers:
        47.721966 - 47.62594 = 0.09602600000000194
        0.09602600000000194 / 47.62594 = 0.0020162541673718554
        which is 0.20162%, which is not great, especially because it is for the number we care about.

      The big numbers on the edges get 20x the precision, but we don't care about them.
        277.16025 - 277.13272 = 0.027530000000012933
        0.027530000000012933 / 277.16025 = 9.932881789510917e-05
        which is 0.0099328 %
       */
      val valueList = dicomImage.pixelData.flatten.distinct.sorted
      Trace.trace("bottom 20: " + valueList.take(20))
      Trace.trace("top 20: " + valueList.takeRight(20))
    }

    val RescaleIntercept = dicomImage.minPixelValue
    val RescaleSlope = (dicomImage.maxPixelValue - dicomImage.minPixelValue) / hiPix

    set(TagByName.RescaleIntercept, RescaleIntercept)
    set(TagByName.RescaleSlope, RescaleSlope)

    def toPix(psmValue: Float): Short = {
      val i = ((psmValue - RescaleIntercept) / RescaleSlope).round
      (i & 0xffff).toShort
    }

    val newPixels = dicomImage.pixelData.flatten.map(toPix).toArray
    val otherWordAttribute = new OtherWordAttribute(TagByName.PixelData)
    otherWordAttribute.setValues(newPixels)
    al.remove(TagByName.PixelData)
    al.put(otherWordAttribute)

    al
  }

  /**
    * Given the DICOM representation of a PSM, convert it to a scaled DICOM image.
    * @param image as DICOM.
    * @return image as DicomImage
    */
  def dicomToImage(image: AttributeList): DicomImage = {

    val RescaleIntercept = image.get(TagByName.RescaleIntercept).getDoubleValues.head
    val RescaleSlope = image.get(TagByName.RescaleSlope).getDoubleValues.head

    val diUnscaled = new DicomImage(image)

    def mapRow(row: IndexedSeq[Float]): IndexedSeq[Float] =
      row.map(value => ((value * RescaleSlope) + RescaleIntercept).toFloat)

    val scaledPixels = diUnscaled.pixelData.map(mapRow)

    new DicomImage(scaledPixels)
  }

}
