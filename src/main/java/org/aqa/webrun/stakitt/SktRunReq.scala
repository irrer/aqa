package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class SktRunReq(rtimageList: Seq[AttributeList], rtplan: AttributeList) extends RunReqClass {
//
}
