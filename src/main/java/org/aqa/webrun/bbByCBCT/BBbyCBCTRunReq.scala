package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import org.aqa.DicomFile
import java.io.File
import org.aqa.db.Machine

case class BBbyCBCTRunReq(rtplanDicomFile: DicomFile, regDicomFile: DicomFile, cbctDicomFile: Seq[DicomFile], machine: Machine) {

  val rtplan = rtplanDicomFile.attributeList.get
  val reg = new ImageRegistration(regDicomFile.attributeList.get)
  val cbct = cbctDicomFile.map(df => df.attributeList.get)

  def reDir(dir: File): BBbyCBCTRunReq = {
    val origDirPath = cbctDicomFile.head.file.getParentFile.getAbsolutePath
    def ifRedir(df: DicomFile) = if (df.file.getParentFile.getAbsolutePath.equalsIgnoreCase(origDirPath)) df.reDir(dir) else df

    val rtplanDicomFile2 = ifRedir(rtplanDicomFile)
    val regDicomFile2 = ifRedir(regDicomFile)
    val cbctDicomFile2 = cbctDicomFile.map(df => df.reDir(dir))

    new BBbyCBCTRunReq(rtplanDicomFile2, regDicomFile2, cbctDicomFile2, machine)
  }
}