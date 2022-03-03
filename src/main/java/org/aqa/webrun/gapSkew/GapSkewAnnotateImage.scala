/*
 * Copyright 2022 Regents of the University of Michigan
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

package org.aqa.webrun.gapSkew

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Util
import org.aqa.webrun.gapSkew.GapSkewUtil._

import java.awt.Color
import java.awt.Polygon
import java.awt.image.BufferedImage

case class GapSkewAnnotateImage(
    dicomImage: DicomImage,
    translator: IsoImagePlaneTranslator,
    topLeft: Leaf,
    topRight: Leaf,
    bottomLeft: Leaf,
    bottomRight: Leaf
) {

  private def d2i = Util.d2i _

  private def arrow(image: BufferedImage, x1: Int, y1: Int, x2: Int, y2: Int, color: Color = Color.darkGray): Unit = {
    val g = ImageUtil.getGraphics(image)
    g.setColor(color)
    ImageUtil.setLineThickness(g, 2)

    g.drawLine(x1, y1, x2, y2)

    val arrowLength = 14
    val arrowWidth = 8
    val ah = arrowWidth / 2

    val arrow: Seq[Array[Int]] = 0 match {
      case _ if (x1 == x2) && (y1 < y2) => // down arrow
        Seq(
          Array(x2 - ah, x2 + ah, x2),
          Array(y2 - arrowLength, y2 - arrowLength, y2)
        )

      case _ if (x1 == x2) && (y1 >= y2) => // up arrow
        Seq(
          Array(x2 - ah, x2 + ah, x2),
          Array(y2 + arrowLength, y2 + arrowLength, y2)
        )

      case _ if (x1 < x2) && (y1 == y2) => // right arrow
        Seq(
          Array(x2 - arrowLength, x2 - arrowLength, x2),
          Array(y2 - ah, y2 + ah, y2)
        )

      case _ if (x1 >= x2) && (y1 == y2) => // left arrow
        Seq(
          Array(x2 + arrowLength, x2 + arrowLength, x2),
          Array(y2 - ah, y2 + ah, y2)
        )

      case _ => throw new RuntimeException("The only arrow directions supported are up, down, right, and left.")
    }

    val p = new Polygon(arrow.head, arrow(1), 3)
    g.fillPolygon(p)
  }

  private def addLeaf(image: BufferedImage, leaf: Leaf, isTop: Boolean): Unit = {
    val g = ImageUtil.getGraphics(image)
    g.setColor(Color.darkGray)

    val y = d2i(translator.iso2PixCoordY(-leaf.yPosition_mm))
    val x1 = d2i(translator.iso2PixCoordX(leaf.xLeftPosition_mm))
    val x2 = d2i(translator.iso2PixCoordX(leaf.xLeftPosition_mm + leaf.width_mm))
    ImageUtil.setLineThickness(g, 3)
    g.drawLine(d2i(x1), d2i(y), d2i(x2), d2i(y))

    // -----------------------------------------------------------------

    ImageText.setFont(g, ImageText.DefaultFont, textPointSize = 20)
    val textHeight = d2i(ImageText.getTextDimensions(g, "Hq").getHeight)
    val textOffsetY = d2i(textHeight * 1.5)
    val textX = d2i(translator.iso2PixCoordX(leaf.xLeftPosition_mm + leaf.width_mm / 2.0))
    val textY = d2i(if (isTop) y - textOffsetY else y + textOffsetY)

    val text = fmt2(leaf.yPosition_mm)

    val textBox = ImageText.getTextDimensions(g, text)
    val margin = 6
    val minX = margin + textBox.getCenterX
    val maxX = dicomImage.width - (margin + textBox.getCenterX)
    val minY = margin + textBox.getCenterY
    val maxY = dicomImage.height - (margin + textBox.getCenterY)

    val tX = 0 match {
      case _ if textX < minX => minX
      case _ if textX > maxX => maxX
      case _                 => textX
    }

    val tY = 0 match {
      case _ if textY < minY => minY
      case _ if textY > maxY => maxY
      case _                 => textY
    }

    ImageText.drawTextCenteredAt(g, tX, tY, text)

    // -----------------------------------------------------------------

    val arrowY1 = {
      if (isTop)
        d2i(textY + textHeight / 2.0)
      else
        d2i(textY - textHeight / 2.0)
    }
    arrow(image, textX, arrowY1, textX, y)
  }

  /**
  Create annotated image.
    */

  def annotate: BufferedImage = {
    val bufferedImage: BufferedImage = dicomImage.toDeepColorBufferedImage(percentToDrop = 0.1)
    // Util.addGraticulesNegY(bufferedImage, translator, Color.yellow)

    addLeaf(bufferedImage, topLeft, isTop = true)
    addLeaf(bufferedImage, topRight, isTop = true)
    addLeaf(bufferedImage, bottomLeft, isTop = false)
    addLeaf(bufferedImage, bottomRight, isTop = false)

    bufferedImage
  }
}
