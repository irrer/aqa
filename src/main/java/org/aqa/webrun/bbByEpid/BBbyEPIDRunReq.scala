package org.aqa.webrun.bbByEpid

import org.aqa.db.Machine
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

/**
 * Data needed to run an EPID BB analysis.
 */
class BBbyEPIDRunReq(alList: Seq[AttributeList]) extends RunReqClass(alList) {

  val epidList = alList.filter(al => Util.modalityOfAl(al).equals("RTIMAGE"))

  val sopOfRTPlan: Option[String] = {
    try {
      val sop = DicomUtil.seqToAttr(alList.head, TagFromName.ReferencedRTPlanSequence).
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

}
