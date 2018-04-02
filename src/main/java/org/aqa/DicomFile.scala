package org.aqa

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.TagFromName
import com.pixelmed.display.ConsumerFormatImageMaker
import edu.umro.ScalaUtil.DicomUtil
import java.awt.image.BufferedImage
import edu.umro.ScalaUtil.Trace

case class DicomFile(file: File) extends Logging {
  private lazy val readResult = Util.readDicomFile(file)
  lazy val valid = readResult.isRight
  lazy val attributeList: Option[AttributeList] = if (readResult.isRight) Some(readResult.right.get) else None
  lazy val error: Option[Throwable] = if (readResult.isLeft) Some(readResult.left.get) else None

  lazy val standardImage: Option[BufferedImage] = {
    if (valid && DicomUtil.isImageStorage(attributeList.get))
      Some(ConsumerFormatImageMaker.makeEightBitImage(attributeList.get))
    else
      None
  }

  lazy val maxContrastImage: Option[BufferedImage] = {
    if (valid && DicomUtil.isImageStorage(attributeList.get)) {
      val pixelList = attributeList.get.get(TagFromName.PixelData).getShortValues.map(s => s & 0xffff).toIndexedSeq
      val lo = pixelList.min
      val hi = pixelList.max
      val range = hi - lo
      val ratio = (255.0 / range).toFloat
      val height = attributeList.get.get(TagFromName.Rows).getIntegerValues.head
      val width = attributeList.get.get(TagFromName.Columns).getIntegerValues.head
      val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

      for (xy <- (0 until (width * height))) {
        val x = xy % width
        val y = xy / width
        val brightness = ((pixelList(xy) - lo) * ratio).floor.toInt & 0xff
        val rgb = DicomFile.rgbTable(brightness)
        image.setRGB(x, y, rgb)
      }
      Some(image)
    } else
      None
  }

  def getImage(contrastModel: DicomFile.ContrastModel.Value): Option[BufferedImage] = {
    contrastModel match {
      case DicomFile.ContrastModel.standard => standardImage
      case DicomFile.ContrastModel.maxContrast => maxContrastImage
      case _ => {
        logger.error(fmtEx(new RuntimeException("Invalid colorScheme: " + contrastModel)))
        None
      }
    }
  }

  def isModality(sopClassUID: String): Boolean = {
    valid && Util.isModality(attributeList.get, sopClassUID)
  }
}

object DicomFile {

  object ContrastModel extends Enumeration {
    val standard = Value
    val maxContrast = Value
  }

  /** Lookup table for brightness -> color. */
  private lazy val rgbTableBW = (0 until 256).map(b => (b << 16) + (b << 8) + b)

  /** Lookup table for brightness -> color. */
  private lazy val rgbTable = (0 until 256).map(b => (b << 8) + b)

  /**
   * Return a list of all the DICOM files in the given directory.  Return the list of all files, DICOM or not.
   */
  def readDicomInDir(dir: File): Seq[DicomFile] = {
    Util.listDirFiles(dir).map(f => new DicomFile(f))
  }

  /**
   * Return a list of DicomFiles that differ by SOPInstanceUID.
   */
  def distinctSOPInstanceUID(dicomFileList: Seq[DicomFile]): Seq[DicomFile] = {
    dicomFileList.filter(df => df.valid).map(df => (Util.sopOfAl(df.attributeList.get), df)).toMap.values.toSeq
  }
}