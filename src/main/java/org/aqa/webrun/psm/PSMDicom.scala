package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage

/**
  * Provide functions to convert a PSM image to and from DICOM.
  *
  * Note that a small degree of precision is lost because the pixels are stored as 16-bit integers.
  * When a PSM analysis is done, a round-trip to DICOM and back is performed, and the largest errors are logged.
  * In testing, the largest error was  2.0861626E-6 == 0.0000020861626   .
  */
object PSMDicom {

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
