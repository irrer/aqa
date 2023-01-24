package org.aqa.webrun.wl

import org.aqa.Config

import java.awt.Font
import java.awt.font.FontRenderContext

/**
  * Support common usage of font used to
  * annotations in graphic and DICOM images.
  */
object GraphicFont {

  def getFont: Font = new Font(Config.WLTextFont, Font.PLAIN, Config.WLTextPointSize)

  def getFontHeight: Int = {
    val allPrintableChars = new String((32 to 126).toList.map(c => c.asInstanceOf[Byte]).toArray)
    val frc = new FontRenderContext(null, true, false)
    val fontHeight = getFont.getStringBounds(allPrintableChars, frc).getHeight
    if (fontHeight.toInt == fontHeight) fontHeight.toInt else fontHeight.toInt + 1
  }

  def getFontRenderContext = new FontRenderContext(null, true, false)

  def main(args: Array[String]): Unit = {}

}
