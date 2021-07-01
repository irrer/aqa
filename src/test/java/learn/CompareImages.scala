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

package learn

import edu.umro.ImageUtil.DicomImage
import org.aqa.DicomFile
import org.aqa.Util

import java.awt.Color
import java.awt.Rectangle
import java.io.File

object CompareImages {

  val oldFile = new File(
    """\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2021Q2\2021-06-29T06-20-02_RTIMAGE_2_1.2.246.352.62.2.5262862586566495996.4529730693056586684\1.2.246.352.62.1.5120024036872846189.16434677943097143484.dcm"""
  )
  val newFile = new File(
    """\\hitspr\e$\Program Files\UMRO\AQAClient\data\DICOMSeries\$TB3_OBI_2021Q2\2021-06-30T06-23-48_RTIMAGE_2_1.2.246.352.62.2.4682063960349851407.2706251095939824060\1.2.246.352.62.1.4651610995792689651.6053999311867058609.dcm"""
  )

  val oldAl = new DicomFile(oldFile).attributeList.get
  val newAl = new DicomFile(newFile).attributeList.get

  val oldImage = new DicomImage(oldAl)
  val newImage = new DicomImage(newAl)

  val oldImageMin = oldImage.minPixelValue
  val oldImageMax = oldImage.maxPixelValue
  val oldImageRange = oldImageMax - oldImageMin

  val newImageMin = newImage.minPixelValue
  val newImageMax = newImage.maxPixelValue
  val newImageRange = newImageMax - newImageMin

  def scaleOld(x: Int, y: Int): Float = {
    (oldImage.get(x, y) - oldImageMin) / oldImageRange
  }

  def scaleNew(x: Int, y: Int): Float = {
    (newImage.get(x, y) - newImageMin) / newImageRange
  }

  def compareRow(y: Int): IndexedSeq[Float] = {
    //val row = (0 until oldImage.width).map(x => scaleOld(x, y) - scaleNew(x, y))
    val row = (0 until oldImage.width).map(x => oldImage.get(x, y) - newImage.get(x, y))
    row
  }

  def main(args: Array[String]): Unit = {

    if (true) {
      val diffArray = (0 until oldImage.height).map(compareRow).toArray
      val diffDicomImage = new DicomImage(diffArray)
      val diffImage = diffDicomImage.toBufferedImage(Color.white)

      val diffFile = new File("""D:\tmp\aqa\tmp\diff.png""")

      Util.writePng(diffImage, diffFile)
      println("Wrote file " + diffFile.getAbsolutePath)

      val closeUpDiff = diffDicomImage.getSubimage(new Rectangle(300, 30, 40, 40))
      val closeUpDiffFile = new File("""D:\tmp\aqa\tmp\diffCloseup.png""")
      val closeupBufDiff = closeUpDiff.toBufferedImage(Color.white)
      Util.writePng(closeupBufDiff, closeUpDiffFile)
      println("Wrote file " + closeUpDiffFile.getAbsolutePath)
    }

    if (true) {
      val center = oldImage.getSubimage(new Rectangle(270, 270, 100, 100))
      //  val center = oldImage.getSubimage(new Rectangle(300, 300, 40, 40))
      val centerBuf = center.toBufferedImage(Color.white)
      val centerFile = new File("""D:\tmp\aqa\tmp\center.png""")
      Util.writePng(centerBuf, centerFile)
      println("Wrote file " + centerFile.getAbsolutePath)
    }
  }

}
