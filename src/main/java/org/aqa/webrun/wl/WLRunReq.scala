package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.run.RunReqClass
import org.aqa.Util
import org.aqa.db.WinstonLutzGeneric

case class WLRunReq(epidList: Seq[AttributeList], rtplan: Option[AttributeList]) extends RunReqClass {

  /**
    * Find the DICOM for the given Winston Lutz.
    * @param wl For this Winston Lutz
    * @return DICOM, if found
    */
  def alOf(wl: WinstonLutzGeneric): Option[AttributeList] = {
    epidList.find(epid => wl.rtimageUID.equals(Util.sopOfAl(epid)))
  }

  def indexOf(al: AttributeList): Int = {
    val uid: String = Util.sopOfAl(al)
    epidList.indexWhere(e => Util.sopOfAl(e) == uid)
  }

  private val firstImageTimeMs = epidList.map(WLImageUtil.timeOfMs).min

  def subDirName(attrList: AttributeList, processing: String): String = {
    val name1 = "%02d".format(indexOf(attrList)) + "-" + imageName(attrList) + s"-$processing"
    val name2 = FileUtil.replaceInvalidFileNameCharacters(name1, '_').replaceAll(" ", "_")
    name2
  }

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
        "%6.1f".format(Util.modulo360(angle)).trim.replaceAll(".0$", "")
      }
    }

    val gantryAngle = DicomUtil.findAllTag(rtimage, TagByName.GantryAngle).head.getDoubleValues.head
    val collimatorAngle = DicomUtil.findAllTag(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head

    val gantryAngle_txt = "G" + angleToString(gantryAngle)
    val collimatorAngle_txt = "C" + angleToString(collimatorAngle)
    val elapsedTime_txt = {
      val min = elapsedTime_ms / (60 * 1000)
      val sec = (elapsedTime_ms / 1000) % 60
      "%d".format(min) + ":" + "%02d".format(sec)
    }

    gantryAngle_txt + " " + collimatorAngle_txt + " " + elapsedTime_txt
  }
}
