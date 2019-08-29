package org.aqa.webrun.bbByEpid

import org.aqa.DicomFile
import com.pixelmed.dicom.AttributeList
import org.aqa.ImageRegistration
import org.aqa.webrun.bbByCBCT.BBbyCBCTRunReq
import java.io.File
import org.aqa.db.Machine
import org.aqa.Util

class BBbyEPIDRunReq(epidListDicomFile: Seq[DicomFile], val machine: Machine) {

  val epidList = Util.sortByDateTime(epidListDicomFile.map(df => df.attributeList).flatten)

  def reDir(dir: File): BBbyEPIDRunReq = {
    val imageReg2 = epidListDicomFile.map(image => image.reDir(dir))
    new BBbyEPIDRunReq(imageReg2, machine)
  }
}
