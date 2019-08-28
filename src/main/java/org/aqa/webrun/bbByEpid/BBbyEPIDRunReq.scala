package org.aqa.webrun.bbByEpid

import org.aqa.DicomFile
import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import org.aqa.webrun.bbByCBCT.BBbyCBCTRunReq
import java.io.File
import org.aqa.db.Machine

case class BBbyEPIDRunReq(rtplanDicomFile: Either[DicomFile, AttributeList], epidList: Seq[DicomFile], machine: Machine) {

  val rtplan = if (rtplanDicomFile.isLeft) rtplanDicomFile.left.get.attributeList.get else rtplanDicomFile.right.get

  def reDir(dir: File): BBbyEPIDRunReq = {
    val imageReg2 = epidList.map(image => image.reDir(dir))

    val plan2 = if (rtplanDicomFile.isLeft)
      Left(rtplanDicomFile.left.get.reDir(dir))
    else
      Right(rtplan)

    new BBbyEPIDRunReq(plan2, imageReg2, machine)
  }
}
