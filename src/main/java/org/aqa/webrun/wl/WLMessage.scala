package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.webrun.wl.WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME
import org.aqa.Logging

import java.io.File

case class WLMessage(runReq: WLRunReq, rtimage: AttributeList) extends Logging {

  val imageName: String = runReq.imageName(rtimage)

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
