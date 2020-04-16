package org.aqa.webrun.bbByEpid

import org.aqa.DicomFile
import java.io.File
import org.aqa.db.Machine
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName

/**
 * Data needed to run an EPID BB analysis.
 */
case class BBbyEPIDRunReq(epidListDicomFile: Seq[DicomFile], val machine: Machine) {

  val epidList = Util.sortByDateTime(epidListDicomFile.map(df => df.attributeList).flatten)

  val sopOfRTPlan: Option[String] = {
    try {
      val sop = DicomUtil.seqToAttr(epidList.head, TagFromName.ReferencedRTPlanSequence).
        head.
        get(TagFromName.ReferencedSOPInstanceUID).
        getSingleStringValueOrNull
      if (sop == null)
        None
      else
        Some(new String(sop))
    } catch {
      case t: Throwable => None
    }
  }

  /**
   * Move files to the given directory.
   */
  def reDir(dir: File): BBbyEPIDRunReq = {
    val epid2 = epidListDicomFile.map(image => image.reDir(dir))
    new BBbyEPIDRunReq(epid2, machine)
  }
}
