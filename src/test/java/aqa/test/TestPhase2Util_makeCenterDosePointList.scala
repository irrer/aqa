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


package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import edu.umro.ScalaUtil.Trace
import org.opensourcephysics.numerics.CubicSpline
import org.aqa.webrun.phase2.leafPosition.LeafPositionCoarseLeafSides
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil
import org.aqa.webrun.phase2.centerDose.CenterDoseAnalysis
import org.aqa.DicomFile
import org.aqa.webrun.phase2.Phase2Util
import java.awt.Point
import java.awt.geom.Point2D

/**
 * Test the TestChase2Util.makeCenterDosePointList function.  Lists the points found and
 * makes images for two different DICOM files with different resolutions.
 */

class TestChase2Util_makeCenterDosePointList extends FlatSpec with Matchers {

  val dir = new File("""src\test\resources""")
  val fileNameList = Seq(
    "TestChase2Util_makeCenterDosePointList1.dcm",
    "TestChase2Util_makeCenterDosePointList2.dcm")

  val outDir = new File("""target\TestChase2Util_makeCenterDosePointList""")
  outDir.mkdirs

  Config.validate

  "TestCenterDoseAnalysis_testConstructCenterDose" should "yield a center dose" in {

    def testFile(fileName: String) = {
      val dicomFile = new DicomFile(new File(dir, fileName))
      println("\n\nfile: " + dicomFile.file.getAbsolutePath)
      val attributeList = dicomFile.attributeList.get
      val start = System.currentTimeMillis
      val pointList = Phase2Util.makeCenterDosePointList(attributeList, new Point2D.Double(0, 0))
      println("Elapsed time in ms: " + (System.currentTimeMillis - start))
      val pointListOffset = Phase2Util.makeCenterDosePointList(attributeList, new Point2D.Double(100, 50))

      val pointGroups = edu.umro.ScalaUtil.Util.sizedGroups(pointList, 20)
      def p2s(p: Point) = {
        p.getX.formatted("%4.0f") + "," + p.getY.formatted("%4.0f")
      }

      println("\n    " + pointGroups.map(g => g.map(p => p2s(p)).mkString(" | ")).mkString("\n    "))
      println("Number of points: " + pointList.size)

      val image = new DicomImage(attributeList)
      val bufImage = image.toDeepColorBufferedImage(Config.DeepColorPercentDrop)

      pointList.map(p => bufImage.setRGB(p.x, p.y, 0))
      pointListOffset.map(p => bufImage.setRGB(p.x, p.y, 0))
      val pngFile = new File(outDir, fileName.replace("dcm", "png"))
      pngFile.delete
      println("Writing image file " + pngFile.getAbsolutePath)
      ImageUtil.writePngFile(bufImage, pngFile)
    }

    fileNameList.map(fn => testFile(fn))

    true should be(true)
  }
}
