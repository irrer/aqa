package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import org.aqa.DicomFile
import java.io.File
import org.aqa.db.Machine
import edu.umro.ScalaUtil.Trace
import org.aqa.run.RunReqClass
import org.aqa.Util

//case class BBbyCBCTRunReq(alList: Seq[AttributeList]) extends RunReqClass(alList) {
case class BBbyCBCTRunReq(rtplan: AttributeList, reg: Option[AttributeList], cbctList: Seq[AttributeList]) extends RunReqClass {

  val imageRegistration: Option[ImageRegistration] = {
    if (reg.isDefined)
      Some(new ImageRegistration(reg.get))
    else
      None
  }

}
