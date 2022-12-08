package org.aqa.webrun.LOC

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass
import org.aqa.webrun.LOCBaseline.LOCBaselineRunReq

case class LOCRunReq(epidList: Seq[AttributeList], baseline: LOCBaselineRunReq) extends RunReqClass {}
