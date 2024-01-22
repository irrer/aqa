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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.io.File
import javax.imageio.ImageIO

object MakeTestLogo {

  def getRawPixels(attributeList: AttributeList): Array[Array[Int]] = {
    val height: Int = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val width: Int = attributeList.get(TagFromName.Columns).getIntegerValues()(0)
    val shorts: Array[Short] = attributeList.get(TagFromName.PixelData).getShortValues
    JavaUtil.pixelDataToArray(height, width, shorts)
  }

  def p2ColorJI(pixel: Float): Int = {
    val range = 0x1ff
    val p = (pixel * range).floor.toInt
    val half = range / 2

    val red = (p >> 1) & 0xff
    val green = (if (pixel < half) (p & 0xff) else (256 - (p >> 8))) & 0xff
    val blue = (0xff - (p & 0xff)) & 0xff
    (red << 16) + (green << 8) + blue
  }

  def p2BW(pixel: Float): Int = {
    val p = ((pixel * 0xff).floor.toInt) & 0xff
    (p << 16) + (p << 8) + p
  }

  def Xp2ColorLR(pixel: Float): Int = {
    val f = pixel
    val a = (1 - f) / 0.2
    val X = Math.floor(a)
    val Y = Math.floor(255 * (a - X))

    var r: Double = 0
    var g: Double = 0
    var b: Double = 0

    X match {
      case 0 => { r = 255; g = Y; b = 0; }
      case 1 => { r = 255 - Y; g = 255; b = 0; }
      case 2 => { r = 0; g = 255; b = Y; }
      case 3 => { r = 0; g = 255 - Y; b = 255; }
      case 4 => { r = Y; g = 0; b = 255; }
      case 5 => { r = 255; g = 0; b = 255; }
      case _ => { r = 0; g = 0; b = 0; } // pixel outside range
    }
    (r.toInt << 16) + (g.toInt << 8) + b.toInt
  }

  def p2ColorLR(pixel: Float): Int = {
    val a = (1 - pixel) / 0.2
    val X = Math.floor(a).toInt
    val Y = Math.floor(255 * (a - X)).toInt

    val rgb = X match {
      case 0 => (255, Y, 0)
      case 1 => (255 - Y, 255, 0)
      case 2 => (0, 255, Y)
      case 3 => (0, 255 - Y, 255)
      case 4 => (Y, 0, 255)
      case 5 => (255, 0, 255)
      case _ => (255, 0, 255) // pixel outside range
    }
    (rgb._1 << 16) + (rgb._2 << 8) + rgb._3
  }

  def write(al: AttributeList, dest: File): Unit = {
    val height: Int = al.get(TagFromName.Rows).getIntegerValues()(0)
    val width: Int = al.get(TagFromName.Columns).getIntegerValues()(0)

    val pix = getRawPixels(al)

    val pixFlat = pix.flatten.sorted

    val min = pixFlat.drop(50).head.toFloat
    val max = pixFlat.reverse.drop(50).head.toFloat
    val range = max - min

    println("min: " + min + "    max: " + max + "   " + dest.getName)

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (row <- (0 until height); col <- (0 until width)) {
      val p =
        if (range == 0) 0
        else {
          val px = pix(row)(col) match {
            case pp if pp < min => min
            case pp if pp > max => max
            case pp             => pp
          }
          (px - min) / range
        }
      //image.setRGB(col, row, p2BW(p))
      image.setRGB(col, row, p2ColorLR(p))
    }
    dest.delete
    ImageIO.write(image, "png", dest)
  }

  def write(dicomFile: File): Unit = {
    //val fileName = dicomFile.getName.toLowerCase.replace(".dcm", "bw.png")
    val fileName = dicomFile.getName.toLowerCase.replace('$', '_').replace(".dcm", "lr.png")
    val imageDir = new File(dicomFile.getParentFile, "image")
    imageDir.mkdirs
    val pngFile = new File(imageDir, fileName)

    val al = new AttributeList
    al.read(dicomFile)
    write(al, pngFile)
  }

  def main(args: Array[String]): Unit = {
    println("starting")

    val blue = new Color(0, 46, 94)
    val yellow = new Color(255, 206, 4)

    val width = 3432
    val height = 752

    val stripe = 128

    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val g = img.getGraphics.asInstanceOf[Graphics2D]

    g.setColor(yellow)

    g.drawLine(0, 0, 50, 50)

    if (false) {
      for (y <- 0 until height)
        for (x <- 0 until width)
          img.setRGB(0, 46, 94)
    }

    g.setColor(yellow)

    for (x <- -width until width) {
      if ((x % (stripe * 2)) < stripe) g.setColor(blue) else g.setColor(yellow)
      g.drawLine(x, 0, x + height, height)
    }

    val outFile = new File("""D:\aqa\tmp\makeTestDb\logoBackground.png""")
    Util.writePng(img, outFile)

    println("Wrote file " + outFile.getAbsolutePath)

    println("finished")
  }

}
