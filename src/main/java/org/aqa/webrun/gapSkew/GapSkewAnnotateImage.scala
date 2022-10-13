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
import org.aqa.db.GapSkew
import org.aqa.webrun.gapSkew.GapSkewUtil._

import java.awt.Color
import java.awt.Polygon
import java.awt.image.BufferedImage

case class GapSkewAnnotateImage(
    dicomImage: DicomImage,
    collimatorAngleRounded_deg: Int,
    beamName: String,
    translator: IsoImagePlaneTranslator,
    minPixel: Float,
    maxPixel: Float,
    topLeft: Leaf,
    topRight: Leaf,
    bottomLeft: Leaf,
    bottomRight: Leaf,
    gapSkew: GapSkew
) {

  private def d2i = Util.d2i _

  /** Number of pixels to draw edge line. */
  private val lineThickness = 2

  private def drawArrow(image: BufferedImage, x1: Int, y1: Int, x2: Int, y2: Int, color: Color = Color.darkGray): Unit = {
    val g = ImageUtil.getGraphics(image)
    g.setColor(color)
    ImageUtil.setLineThickness(g, lineThickness)

    g.drawLine(x1, y1, x2, y2)

    val arrowLength = 14
    val arrowWidth = 8
    val ah = arrowWidth / 2

    val arrowPoints: Seq[Array[Int]] = 0 match {
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

    val p = new Polygon(arrowPoints.head, arrowPoints(1), 3)
    g.fillPolygon(p)
  }

  private def addBeamName(image: BufferedImage): Unit = {
    val g = ImageUtil.getGraphics(image)
    g.setColor(Color.darkGray)

    ImageText.setFont(g, ImageText.DefaultFont, textPointSize = 30)
    val centerX = dicomImage.width / 2
    val centerY = ImageText.getTextDimensions(g, beamName).getHeight
    ImageText.drawTextCenteredAt(g, centerX, centerY, beamName)
  }

  /**
    * Annotate one of the four measurement points.
    * @param image Write on this image.
    * @param leaf Which leaf.
    * @param isTop True if this is a top edge, false if bottom.
    */
  private def annotateLeaf(image: BufferedImage, leaf: Leaf, isTop: Boolean): Unit = {
    addBeamName(image)
    val g = ImageUtil.getGraphics(image)
    g.setColor(Color.white)

    val y = d2i(translator.iso2PixCoordY(-leaf.yPosition_mm))
    val x1 = d2i(translator.iso2PixCoordX(leaf.xLeftPosition_mm))
    val x2 = d2i(translator.iso2PixCoordX(leaf.xLeftPosition_mm + leaf.width_mm))
    ImageUtil.setLineThickness(g, 3)
    g.drawLine(d2i(x1), d2i(y), d2i(x2), d2i(y))

    g.setColor(Color.darkGray)
    ImageText.setFont(g, ImageText.DefaultFont, textPointSize = 20)
    val textHeight = d2i(ImageText.getTextDimensions(g, "Hq").getHeight)
    val textOffsetY = d2i(textHeight * 1.5)
    val textX = d2i(translator.iso2PixCoordX(leaf.xLeftPosition_mm + leaf.width_mm / 2.0))
    val textY = d2i(if (isTop) y - textOffsetY - lineThickness else y + textOffsetY + lineThickness)

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
        d2i(textY + textHeight / 2.0) - lineThickness
      else
        d2i(textY - textHeight / 2.0) + lineThickness
    }
    drawArrow(image, textX, arrowY1, textX, y)
  }

  /**
    * Add a description to an edge
    * @param bufferedImage To this image.
    * @param isTop True if top edge should be annotated, false if bottom edge.
    */
  private def annotateEdgeDescription(bufferedImage: BufferedImage, isTop: Boolean): Unit = {
    val g = ImageUtil.getGraphics(bufferedImage)
    ImageText.setFont(g, ImageText.DefaultFont, textPointSize = 30)
    val fontHeight = ImageText.getFontHeight(g)

    val x_pix = translator.iso2PixCoordX((topLeft.xCenter_mm + topRight.xCenter_mm) / 2)

    val yOffset_pix = translator.pix2IsoDistY(fontHeight) * 2.5

    val y_pix = {
      if (isTop)
        translator.iso2PixCoordY(-gapSkew.topLeftValue_mm.get) - yOffset_pix
      else
        translator.iso2PixCoordY(-gapSkew.bottomLeftValue_mm.get) + yOffset_pix
    }

    val edgeType =
      if (isTop)
        gapSkew.topLeftEdgeType
      else
        gapSkew.bottomLeftEdgeType

    val text = {
      val x = "X" + edgeType.bank

      val mlc = {
        "MLC" + " " + {
          if (edgeType.bank == 1) "BBank" else "ABank"
        }
      }

      val jawOrMlc = {
        if (edgeType.isJaw)
          "Jaw"
        else
          mlc
      }

      x + " " + jawOrMlc
    }

    g.setColor(Color.red)
    ImageText.drawTextCenteredAt(g, x_pix, y_pix, text)
  }

  /**
    * Create annotated image.
    */

  def annotate: BufferedImage = {
    val bufferedImage: BufferedImage = dicomImage.toDeepColorBufferedImage(minPixel, maxPixel)
    Util.addGraticulesNegY(bufferedImage, translator, Color.yellow)

    annotateLeaf(bufferedImage, topLeft, isTop = true) // , isTop = !colIs270) // true)
    annotateLeaf(bufferedImage, topRight, isTop = true) // , isTop = !colIs270) // true)
    annotateLeaf(bufferedImage, bottomLeft, isTop = false) // , isTop = colIs270) // false)
    annotateLeaf(bufferedImage, bottomRight, isTop = false) // , isTop = colIs270) // false)

    annotateEdgeDescription(bufferedImage, isTop = true)
    annotateEdgeDescription(bufferedImage, isTop = false)

    bufferedImage
  }
}
