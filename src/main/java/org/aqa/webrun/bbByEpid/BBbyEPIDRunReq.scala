package org.aqa.webrun.bbByEpid

import org.aqa.DicomFile
import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import org.aqa.webrun.bbByCBCT.BBbyCBCTRunReq
import java.io.File
import org.aqa.db.Machine

object BBbyEPIDRunReq {
  case class ImageReg(epidFile: DicomFile, regFile: DicomFile) {
    val epid = epidFile.attributeList.get
    val reg = regFile.attributeList.get
    val imageRegistration = new ImageRegistration(reg)

    def reDir(dir: File) = new ImageReg(epidFile.reDir(dir), regFile.reDir(dir))
  }
}

case class BBbyEPIDRunReq(rtplanDicomFile: Either[DicomFile, AttributeList], imageReg: Seq[BBbyEPIDRunReq.ImageReg], machine: Machine) {

  val rtplan = if (rtplanDicomFile.isLeft) rtplanDicomFile.left.get.attributeList.get else rtplanDicomFile.right.get

  def reDir(dir: File): BBbyEPIDRunReq = {
    val origDirPath = imageReg.head.epidFile.file.getParentFile.getAbsolutePath
    def ifRedir(df: DicomFile) = if (df.file.getParentFile.getAbsolutePath.equalsIgnoreCase(origDirPath)) df.reDir(dir) else df

    val imageReg2 = imageReg.map(ir => ir.reDir(dir))

    val plan2 = if (rtplanDicomFile.isLeft)
      Left(rtplanDicomFile.left.get.reDir(dir))
    else
      Right(rtplan)

    new BBbyEPIDRunReq(plan2, imageReg2, machine)
  }
}
