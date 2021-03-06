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


package aqa.test

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeFactory
import java.awt.geom.Point2D
import org.scalactic.source.Position.apply
import scala.collection.Seq
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.ScalaUtil.Trace._
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import edu.umro.DicomDict.TagByName

/**
 * Test IsoImagePlaneTranslator.
 *
 */

class TestSymmetryAndFlatnessAnalysisPixelMap extends FlatSpec with Matchers {

  private val al = new AttributeList

  private def put(tag: AttributeTag, valueList: Seq[String]): Unit = {
    val a = AttributeFactory.newAttribute(tag)
    val vr = a.getVR

    def add(v: String) = {
      vr match {
        case _ if ValueRepresentation.isDecimalStringVR(vr) => a.addValue(v.toDouble)
        case _ if ValueRepresentation.isIntegerStringVR(vr) => a.addValue(v.toInt)
        case _ => a.addValue(v)
      }
    }
    valueList.map(v => add(v))
    al.put(a)
  }

  val width = 1190
  val height = 1190

  private def put(tag: AttributeTag, value: String): Unit = put(tag, Seq(value))

  put(TagByName.RTImageSID, "1500.01778082533")
  put(TagByName.RadiationMachineSAD, "1000")
  put(TagFromName.Rows, width.toString)
  put(TagFromName.Columns, height.toString)
  put(TagByName.ImagePlanePixelSpacing, Seq("0.336", "0.336"))

  "SymmetryAndFlatnessAnalysisPixelMap" should "make spots on image" in {

    val translator = new IsoImagePlaneTranslator(al)

    //    val pixMap = SymmetryAndFlatnessAnalysisPixelMap.getPixelMap(al)
    //    trace
    //
    //    // setting this to true shows an image
    //    if (false) {
    //
    //      import java.awt.FlowLayout
    //      import java.awt.image.BufferedImage
    //      import java.io.File
    //      import java.io.IOException
    //      import javax.imageio.ImageIO
    //      import javax.swing.ImageIcon
    //      import javax.swing.JFrame
    //      import javax.swing.JLabel
    //
    //      val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    //      val rgb = Color.CYAN.getRGB
    //
    //      pixMap.values.flatten.map(p => img.setRGB(p.getX.round.toInt, p.getY.round.toInt, rgb))
    //      val icon = new ImageIcon(img)
    //      val frame = new JFrame()
    //      frame.setLayout(new FlowLayout)
    //      frame.setSize(width + 20, height + 20)
    //      val lbl = new JLabel
    //      lbl.setIcon(icon)
    //      frame.add(lbl)
    //      frame.setVisible(true)
    //      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    //      trace
    //    }
  }

  val j = 5
  (j == 5) should be(true)
}
