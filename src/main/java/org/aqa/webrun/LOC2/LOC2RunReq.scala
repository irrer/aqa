package org.aqa.webrun.LOC2

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class LOC2RunReq(epidList: Seq[AttributeList]) extends RunReqClass {}
