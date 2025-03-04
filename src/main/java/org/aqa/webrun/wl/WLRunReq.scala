package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.db.WinstonLutz
import org.aqa.run.RunReqClass
import org.aqa.Util

case class WLRunReq(epidList: Seq[AttributeList], rtplan: Option[AttributeList]) extends RunReqClass {

  /**
   * Find the DICOM for the given Winston Lutz.
   * @param wl For this Winston Lutz
   * @return DICOM, if found
   */
  def alOf(wl: WinstonLutz): Option[AttributeList] = {
    epidList.find(epid => wl.rtimageUID.equals(Util.sopOfAl(epid)))
  }
}
