package org.aqa.webrun.bbByEpid

import org.aqa.db.Machine
import org.aqa.Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass
import edu.umro.DicomDict.TagByName

/**
 * Data needed to run an EPID BB analysis.
 */
case class BBbyEPIDRunReq(epidList: Seq[AttributeList]) extends RunReqClass {

  val sopOfRTPlan: Option[String] = {
    try {
      val sop = DicomUtil.seqToAttr(epidList.head, TagByName.ReferencedRTPlanSequence).
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
