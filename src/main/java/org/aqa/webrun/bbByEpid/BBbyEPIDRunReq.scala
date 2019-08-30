package org.aqa.webrun.bbByEpid

import org.aqa.DicomFile
import java.io.File
import org.aqa.db.Machine
import org.aqa.Util

/**
 * Data needed to run an EPID BB analysis.
 */
class BBbyEPIDRunReq(epidListDicomFile: Seq[DicomFile], val machine: Machine) {

  val epidList = Util.sortByDateTime(epidListDicomFile.map(df => df.attributeList).flatten)

  /**
   * Move files to the given directory.
   */
  def reDir(dir: File): BBbyEPIDRunReq = {
    val imageReg2 = epidListDicomFile.map(image => image.reDir(dir))
    new BBbyEPIDRunReq(imageReg2, machine)
  }
}
