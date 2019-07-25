package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration

case class BBbyCBCTRunReq(rtplan: AttributeList, reg: ImageRegistration, cbct: Seq[AttributeList]) {

}