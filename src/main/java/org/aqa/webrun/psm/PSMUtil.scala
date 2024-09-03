package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.db.PixelSensitivityMatrix

object PSMUtil {
  def correctImage(wholeDetectorImage: DicomImage, floodField: DicomImage, pixelSensitivityMatrix: PixelSensitivityMatrix): AttributeList = {
    ???
  }

  def correctImage(wholeDetectorImage: AttributeList, floodField: AttributeList, pixelSensitivityMatrix: PixelSensitivityMatrix): AttributeList = {
    correctImage( new DicomImage(wholeDetectorImage),   new DicomImage(wholeDetectorImage) , pixelSensitivityMatrix)
  }
}
