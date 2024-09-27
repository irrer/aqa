package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class PSMRunReq(rtplan: AttributeList, wholeDetector: AttributeList, rtimageList: Seq[AttributeList], floodField: AttributeList) extends RunReqClass {

  case class Beam(top: Double, bottom: Double, left: Double, right: Double) {
    val width: Double = (right - left).abs
    val height: Double = (bottom - top).abs
  }
}
