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

import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util

import java.awt.Color
import java.awt.Rectangle
import java.io.File

object EpidNoise extends Logging {

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    def doit(dir: File): Unit = {

      println("Processing " + dir.getName)

      val outDir = dir.listFiles().filter(_.getName.startsWith("output_")).head

      val epidHtml = outDir.listFiles().filter(_.getName.equals("EPID_0.html")).head
      val epidText = Util.readTextFile(epidHtml).right.get

      val displayHtml = outDir.listFiles().filter(_.getName.equals("display.html")).head
      val displayText = Util.readTextFile(displayHtml).right.get

      val gantryAngle = {
        val prefix = "<h2>Gantry Angle "
        val ga = epidText.drop(epidText.indexOf(prefix) + prefix.length).split(" ").head.toInt
        ga
      }

      val noise = {
        val prefix1 = "BB Signal to Noise:"
        val t1 = displayText.drop(displayText.indexOf(prefix1) + prefix1.length)
        val t2 = t1.split(">")(1).split("<").head
        t2.toDouble
      }

      val al = {
        val rtImageList = dir.listFiles().map(f => DicomFile(f)).filter(df => df.attributeList.isDefined).map(_.attributeList.get).filter(Util.isRtimage)
        rtImageList.filter(al => Util.angleRoundedTo90(Util.gantryAngle(al)) == gantryAngle).head
      }

      val mv: Double = {
        try {
          DicomUtil.findAllSingle(al, TagByName.KVP).head.getDoubleValues.head / 1000.0
        } catch {
          case _: Throwable => -1
        }
      }

      val dicomImage = new DicomImage(al)

      val trans = new IsoImagePlaneTranslator(al)

      val dim = 10

      val x = trans.iso2PixCoordX(-dim / 2).round.toInt
      val y = trans.iso2PixCoordY(-dim / 2).round.toInt
      val width = trans.iso2PixDistX(dim).round.toInt
      val height = trans.iso2PixDistY(dim).round.toInt

      val rect = new Rectangle(x, y, width, height)

      val closeUp = dicomImage.getSubimage(rect)

      val scale = 10

      val minPix = closeUp.minPixelValue //  closeUp.pixelData.flatten.sorted.drop(5).head
      val maxPix = closeUp.pixelData.flatten.sorted.dropRight(2).last
      val smallImg = closeUp.toBufferedImage(ImageUtil.rgbColorMap(Color.white), minPix, maxPix)

      val img = ImageUtil.magnify(smallImg, scale)

      val file = new File(dir, s"${dir.getName}_Noise_${"%4.1f".format(noise)}_${mv}_MV.png")

      Util.writePng(img, file)
      println(s"wrote file ${file.getAbsolutePath}")

      println(
        s"${dir.getName} gantry: ${"%3d"
          .format(gantryAngle)} noise: ${"%5.2f".format(noise)}    rows: ${dicomImage.Rows}   columns: ${dicomImage.Columns}    minPix: $minPix  maxPix: $maxPix   pixRange: ${"%4.0f"
          .format(maxPix - minPix)}   MV: $mv"
      )

      println(dir)
    }

    val mainDir = new File("D:/tmp/noise/images")

    mainDir
      .listFiles()
      .foreach(d => {
        try {
          doit(d)
        } catch {
          case t: Throwable =>
            println(fmtEx(t))
        }
      })

    def elapsed: String = (System.currentTimeMillis - start).formatted("%8d    ")
    println(s"Exiting.  elapsed $elapsed")
    System.exit(0)
  }

}
