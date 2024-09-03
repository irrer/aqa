package org.aqa.webrun.floodField

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

/** Validated flood field data. */
case class FloodRunReq(floodField: AttributeList) extends RunReqClass {}
