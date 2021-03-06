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

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringAnalysis
import org.aqa.webrun.phase2.Phase2Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.awt.image.BufferedImage
import java.io.File

/**
  * Test the LeafPositionAnalysis.leafEnds method.
  */

class TestCollimatorCenteringAnalysis_analyze extends FlatSpec with Matchers {

  Config.validate

  private val dir = new File("""src\test\resources\TestCollimatorCentering""")
  private val fileNameList = Seq(("TestCollimatorCentering090a.dcm", "TestCollimatorCentering270a.dcm"), ("TestCollimatorCentering090b.dcm", "TestCollimatorCentering270b.dcm"))

  private val outDir = new File("""target\TestCollimatorCenteringAnalysis""")
  outDir.mkdirs

  private val rtplan = {
    val file = new File(dir, "TestCollimatorCenteringRtplan.dcm")
    val al = new AttributeList
    al.read(file)
    al
  }

  "TestCollimatorCenteringAnalysis_analyze" should "calculate collimator center" in {

    def writePng(fileName: String, bufImage: BufferedImage) = {
      val pngFile = new File(outDir, fileName.replace("dcm", "png"))
      pngFile.delete
      println("Writing image file " + pngFile.getAbsolutePath)
      ImageUtil.writePngFile(bufImage, pngFile)
    }

    def correctBadPix(dicomFile: DicomFile): DicomImage = {
      val al = dicomFile.attributeList.get
      val translator = new IsoImagePlaneTranslator(al)
      val radius = translator.iso2PixDistX(Config.BadPixelRadius_mm).round.toInt
      val imageOrig = new DicomImage(dicomFile.attributeList.get)
      val image = imageOrig.correctBadPixels(Phase2Util.identifyBadPixels(imageOrig, radius), radius)
      image
    }

    def testFilePair(fileName090: String, fileName270: String): Unit = {
      val dicomFile090 = new DicomFile(new File(dir, fileName090))
      val dicomFile270 = new DicomFile(new File(dir, fileName270))
      println("\n\nfiles:\n    " + dicomFile090.file.getAbsolutePath + "\n    " + dicomFile270.file.getAbsolutePath)

      val image090 = correctBadPix(dicomFile090)
      val image270 = correctBadPix(dicomFile270)

      val analysisResult = CollimatorCenteringAnalysis.testAnalyze(dicomFile090.attributeList.get, dicomFile270.attributeList.get, image090, image270, -1, rtplan)

      val collimatorCentering = analysisResult.collimatorCentering
      val bufImage090 = analysisResult.measureTBLREdges090
      val bufImage270 = analysisResult.measureTBLREdges270

      writePng(fileName090, bufImage090.bufferedImage)
      writePng(fileName270, bufImage270.bufferedImage)

      println("centerDose: " + collimatorCentering)
    }

    fileNameList.foreach(fn => testFilePair(fn._1, fn._2))

    true should be(true)
  }
}
