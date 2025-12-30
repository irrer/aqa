package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.webrun.wl.WLProcessImage.DIAGNOSTICS_TEXT_FILE_NAME
import org.aqa.Logging

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

case class WLMessage(runReq: WLRunReq, rtimage: AttributeList) extends Logging {

  val imageName: String = runReq.imageName(rtimage)

  private val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  private val textBuffer = new StringBuffer()

  private def put(level: String, text: String): Unit = {
    val dateText = Util.formatDate(format, new Date)
    val fullMsg = s"$dateText $level $text\n"
    textBuffer.synchronized(textBuffer.append(fullMsg))
  }

  def info(text: String): Unit = {
    logger.info(s"$imageName: $text")
    put("INFO", text)
  }

  def warn(text: String): Unit = {
    logger.error(s"$imageName: $text")
    put("WARN", text)
  }

  def error(text: String): Unit = {
    logger.error(s"$imageName: $text")
    put("ERROR", text)
  }

  override def toString: String = textBuffer.toString

  def save(dir: File): Unit = {
    Util.writeFile(new File(dir, DIAGNOSTICS_TEXT_FILE_NAME), this.toString)
  }

}
