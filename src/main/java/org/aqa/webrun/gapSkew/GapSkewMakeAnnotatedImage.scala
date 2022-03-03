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

import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Util

import java.awt.Color
import java.awt.image.BufferedImage

object GapSkewMakeAnnotatedImage {

  /**
    * Mark the leaf ends and label with their positions.
    *
    * @param left_pix     Leftmost position of marker line in pixels
    * @param right_pix    Rightmost position of marker line in pixels
    * @param position_pix Vertical position of leaf end in pixels.
    */
  private def annotateMeasurement(left_pix: Double, right_pix: Double, position_pix: Double, translator: IsoImagePlaneTranslator, image: BufferedImage): Unit = {
    val graphics = ImageUtil.getGraphics(image)

    val pos = Util.d2i(position_pix)

    // draw white+black+white lines
    val x1 = Util.d2i(left_pix)
    val x2 = Util.d2i(right_pix)

    graphics.setColor(Color.black)
    graphics.drawLine(x1, pos, x2, pos)
    graphics.setColor(Color.white)
    graphics.drawLine(x1, pos - 1, x2, pos - 1)
    graphics.drawLine(x1, pos + 1, x2, pos + 1)

    val text = (-translator.pix2IsoCoordY(position_pix)).formatted("%6.2f").trim
    val textDim = ImageText.getTextDimensions(graphics, text)

    val boundingBoxHeight = translator.iso2PixDistY(Config.GapSkewLeafSideFinding_mm)

    val border_px = 3

    // center coordinates for text
    val xCenter = (left_pix + right_pix) / 2.0
    val yCenter = position_pix - 5 - boundingBoxHeight - textDim.getCenterY - border_px * 2

    def makeBackground(): Unit = {
      val x = xCenter - textDim.getWidth / 2.0 - border_px
      val y = yCenter - textDim.getHeight / 2.0 - border_px
      val width = textDim.getWidth + border_px * 2
      val height = textDim.getHeight + border_px * 2
      graphics.setColor(Color.black)
      graphics.fillRect(Util.d2i(x), Util.d2i(y), Util.d2i(width), Util.d2i(height))
    }

    makeBackground()
    graphics.setColor(Color.white)
    ImageText.drawTextCenteredAt(graphics, xCenter, yCenter, text)

  }
}
