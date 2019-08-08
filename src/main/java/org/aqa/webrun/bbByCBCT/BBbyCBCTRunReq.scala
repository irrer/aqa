package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import org.aqa.DicomFile
import java.io.File
import org.aqa.db.Machine

case class BBbyCBCTRunReq(rtplanDicomFile: Either[DicomFile, AttributeList], regDicomFile: Option[DicomFile], cbctDicomFile: Seq[DicomFile], machine: Machine) {

  val rtplan = if (rtplanDicomFile.isLeft) rtplanDicomFile.left.get.attributeList.get else rtplanDicomFile.right.get

  val reg: Option[ImageRegistration] = {
    if (regDicomFile.isDefined)
      Some(new ImageRegistration(regDicomFile.get.attributeList.get))
    else
      None
  }
  val cbct = cbctDicomFile.map(df => df.attributeList.get)

  def reDir(dir: File): BBbyCBCTRunReq = {
    val origDirPath = cbctDicomFile.head.file.getParentFile.getAbsolutePath
    def ifRedir(df: DicomFile) = if (df.file.getParentFile.getAbsolutePath.equalsIgnoreCase(origDirPath)) df.reDir(dir) else df

    val cbctDicomFile2 = cbctDicomFile.map(df => df.reDir(dir))
    val regDicomFile2 = ifRedir(regDicomFile.get)

    val plan = if (rtplanDicomFile.isLeft)
      Left(rtplanDicomFile.left.get.reDir(dir))
    else
      Right(rtplan)

    new BBbyCBCTRunReq(plan, Some(regDicomFile2), cbctDicomFile2, machine)
  }
}
