package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.webrun.wl.WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME
import org.aqa.Logging

import java.io.File

case class WLMessage(extendedData: ExtendedData, rtimage: AttributeList) extends Logging {

  val elapsedTime_ms: Long = {
    val ms = Util.extractDateTimeAndPatientIdFromDicomAl(rtimage)._1.head.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  val imageName: String = {
    val gantryRounded_deg = Util.angleRoundedTo5(Util.gantryAngle(rtimage))
    val collimatorRounded_deg = Util.angleRoundedTo5(Util.collimatorAngle(rtimage))
    val tableAngleRounded_deg = Util.angleRoundedTo5(rtimage.get(TagByName.PatientSupportAngle).getDoubleValues.head)

    val fmt = "%03d"

    val gantryRounded_txt = "G" + fmt.format(gantryRounded_deg)
    val collimatorRounded_txt = "C" + fmt.format(collimatorRounded_deg)
    val tableAngleRounded_txt = "T" + fmt.format(tableAngleRounded_deg)
    val elapsedTime_txt = {
      val min = elapsedTime_ms / (60 * 1000)
      val sec = (elapsedTime_ms / 1000) % 60
      min.formatted("%d") + ":" + sec.formatted("%02d")
    }

    s"$gantryRounded_txt $collimatorRounded_txt $tableAngleRounded_txt $elapsedTime_txt"
  }

  private val textBuffer = new StringBuffer()

  def info(msg: String): Unit = {
    logger.info(s"$imageName: $msg")
    textBuffer.append(s"$msg\n")
  }

  def error(msg: String): Unit = {
    logger.error(s"$imageName: $msg")
    textBuffer.append(s"Error: $msg\n")
  }

  def save(subDir: File): Unit = {
    Util.writeFile(new File(subDir, DIAGNOSTICS_TEXT_FILE_NAME), textBuffer.toString)
  }

}
