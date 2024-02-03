package org.aqa.webrun.psm.cmn

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class CmnRunReq(rtplan: AttributeList, rtimageMap: Map[String, AttributeList]) extends RunReqClass {}
