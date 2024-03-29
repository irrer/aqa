/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.display.ConsumerFormatImageMaker
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil

import java.awt.image.BufferedImage
import java.io.File

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
      case DicomFile.ContrastModel.standard    => standardImage
      case DicomFile.ContrastModel.maxContrast => maxContrastImage
      case _ => {
        logger.error(fmtEx(new RuntimeException("Invalid colorScheme: " + contrastModel)))
        None
      }
    }
  }

  def isRtplan: Boolean = valid && Util.isRtplan(attributeList.get)
  def isRtimage: Boolean = valid && Util.isRtimage(attributeList.get)
  def isCt: Boolean = valid && Util.isCt(attributeList.get)
  def isReg: Boolean = valid && Util.isReg(attributeList.get)

  def reDir(dir: File) = new DicomFile(Util.reDir(file, dir))

  lazy val originalDicomImage: Option[DicomImage] = {
    attributeList match {
      case Some(al) => Some(new DicomImage(attributeList.get))
      case _        => None
    }
  }

  private lazy val badPixelList: Option[Seq[DicomImage.PixelRating]] = {
    originalDicomImage match {
      case Some(odi) => {
        val numPixels = odi.width * odi.height
        val million = 1000.0 * 1000
        val sampleSize = ((Config.BadPixelSamplePerMillion / million) * numPixels).round.toInt
        val maxBadPixels = ((Config.MaxEstimatedBadPixelPerMillion / million) * numPixels).round.toInt
        val badPixels = odi.identifyBadPixels(maxBadPixels, Config.BadPixelStdDev, Config.BadPixelMaximumPercentChange, Util.badPixelRadius(attributeList.get), Config.BadPixelMinimumDeviation_CU)
        Some(badPixels.filter(bp => bp.rating > 100)) // TODO filter?
      }
      case _ => None
    }
  }

  lazy val correctedDicomImage: Option[DicomImage] = {
    (originalDicomImage, badPixelList) match {
      case (Some(odi), Some(bpl)) => Some(odi.correctBadPixels(bpl, Util.badPixelRadius(attributeList.get)))
      case _                      => None
    }
  }
}

object DicomFile extends Logging {

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
    * Return a list of DicomFiles that ha SOPInstanceUID.
    */
  def distinctSOPInstanceUID(dicomFileList: Seq[DicomFile]): Seq[DicomFile] = {
    val after = dicomFileList.filter(df => df.valid).map(df => (Util.sopOfAl(df.attributeList.get), df)).toMap.values.toSeq
    after // TODO rm
  }

}
