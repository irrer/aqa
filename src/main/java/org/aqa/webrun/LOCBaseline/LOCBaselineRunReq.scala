package org.aqa.webrun.LOCBaseline

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class LOCBaselineRunReq(baselineOpen: AttributeList, baselineTrans: AttributeList) extends RunReqClass {}
