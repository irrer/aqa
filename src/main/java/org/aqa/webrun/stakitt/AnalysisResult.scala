package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import org.aqa.webrun.ExtendedData

case class AnalysisResult(extendedData: ExtendedData, rtimage: AttributeList, rtplan: Option[AttributeList]) {
  //
}
