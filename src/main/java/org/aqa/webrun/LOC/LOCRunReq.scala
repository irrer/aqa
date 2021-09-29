package org.aqa.webrun.LOC

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class LOCRunReq(epidList: Seq[AttributeList]) extends RunReqClass {}
