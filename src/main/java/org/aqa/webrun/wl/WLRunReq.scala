package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.run.RunReqClass

case class WLRunReq(epidList: Seq[AttributeList]) extends RunReqClass {
  /*
  val sopOfRTPlan: Option[String] = {
    try {
      val sop = DicomUtil.seqToAttr(epidList.head, TagByName.ReferencedRTPlanSequence).head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull
      if (sop == null)
        None
      else
        Some(new String(sop))
    } catch {
      case _: Throwable => None
    }
  }
   */
}
