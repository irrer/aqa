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

import org.aqa.Util

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.io.File

object MakeTestLogo {

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
