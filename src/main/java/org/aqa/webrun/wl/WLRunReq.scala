package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class WLRunReq(epidList: Seq[AttributeList], rtplan: Option[AttributeList]) extends RunReqClass {
}
