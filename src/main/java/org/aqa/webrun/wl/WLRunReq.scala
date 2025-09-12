package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
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

  private val firstImageTimeMs = epidList.map(WLImageUtil.timeOfMs).min

  /**
    * Make a name for the image that will make sense to the user.
    * @param rtimage For this DICOM image.
    * @return A nice name.
    */
  def imageName(rtimage: AttributeList): String = {

    val elapsedTime_ms: Long = WLImageUtil.timeOfMs(rtimage) - firstImageTimeMs

    def angleToString(angle: Double): String = {

      if (WLImageUtil.isCardinalAngle(angle))
        "%03d".format(Util.angleRoundedTo90(angle))
      else {
        "%6.2f".format(Util.modulo360(angle))
      }
    }

    val gantryAngle = DicomUtil.findAllSingle(rtimage, TagByName.GantryAngle).head.getDoubleValues.head
    val collimatorAngle = DicomUtil.findAllSingle(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head

    val gantryAngle_txt = "G" + angleToString(gantryAngle)
    val collimatorAngle_txt = "C" + angleToString(collimatorAngle)
    val elapsedTime_txt = {
      val min = elapsedTime_ms / (60 * 1000)
      val sec = (elapsedTime_ms / 1000) % 60
      min.formatted("%d") + ":" + sec.formatted("%02d")
    }

    gantryAngle_txt + " " + collimatorAngle_txt + " " + elapsedTime_txt
  }
}
