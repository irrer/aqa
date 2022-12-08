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

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnnotateImage
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test the LeafPositionAnalysis.
  *
  */

class TestLeafPositionAnalysis extends FlatSpec with Matchers {
  "LeafPositionAnalysis" should "measure leaf positions" in {
    val dir = new File("""src\test\resources\TestLeafPositionAnalysis""")
    val file = new File(dir, """rtimage1.dcm""")

    val outDir = new File("""target\TestLeafPositionAnalysisOverview""")
    Util.deleteFileTreeSafely(outDir)
    outDir.mkdirs

    val imageAl = Util.readDicomFile(file).right.get
    val dicomImage = {
      val orig = new DicomImage(imageAl)
      val correctRadius = 5
      val correctedImage = orig.correctBadPixels(orig.identifyBadPixels(100, 2.0, 50, correctRadius, 100), correctRadius)
      correctedImage
    }

    val planFile = new File(dir, """rtplan1.dcm""")
    val planAl = Util.readDicomFile(planFile).right.get

    val beamName = "PF Stat 0"

    val outputPK = -1.toLong

    val collimatorCentering = new CollimatorCentering(
      collimatorCenteringPK = None,
      outputPK = -1,
      status = "pass",
      SOPInstanceUID090 = "12.34",
      SOPInstanceUID270 = "12.345",
      gantryAngleRounded_deg = 0,
      "beam090",
      "beam270",
      xCollimatorCenter_mm = 0,
      yCollimatorCenter_mm = 0,
      X1_090_mm = 0,
      X2_090_mm = 0,
      Y1_090_mm = 0,
      Y2_090_mm = 0,
      X1_270_mm = 0,
      X2_270_mm = 0,
      Y1_270_mm = 0,
      Y2_270_mm = 0
    )
    val start = System.currentTimeMillis()
    val leafPositionList = LeafPositionAnalysis.testMeasureBeam(beamName, outputPK, imageAl, dicomImage, planAl, collimatorCentering)
    val elapsed = System.currentTimeMillis() - start

    leafPositionList.foreach(lp => println(lp))

    val avg = leafPositionList.map(lp => lp.offset_mm.abs).sum / leafPositionList.size
    println("LeafPositionIsolationDistance_mm: " + Config.LeafPositionIsolationDistance_mm + "    Average (absolute value of) offset: " + avg)

    val translator = new IsoImagePlaneTranslator(imageAl)
    val horizontal = true
    val leafWidthList_mm = Seq(5.0)
    val bufImg = LeafPositionAnnotateImage.annotateImage(leafPositionList, horizontal, dicomImage, leafWidthList_mm, translator)

    val pngFile = new File(outDir, "TestLeafPositionAnalysis.png")
    pngFile.delete
    println("A collimator centering position of 0,0 was assumed")
    println("Writing image file: " + pngFile.getAbsolutePath)
    ImageUtil.writePngFile(bufImg, pngFile)
    println("Elapsed calculation time in ms: " + elapsed)

    true should be(true)
  }
}
