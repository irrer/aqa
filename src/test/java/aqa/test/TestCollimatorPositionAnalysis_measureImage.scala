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

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqa.webrun.phase2.collimatorPosition.CollimatorPositionAnalysis
import java.awt.Point
import java.io.File
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Config
import java.awt.Rectangle
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.MeasureTBLREdges
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName

/**
 * Test the Config.
 *
 */

class TestCollimatorPositionAnalysis_measureImage extends FlatSpec with Matchers {

  Config.validate

  val dir = new File("""src\test\resources\TestCollimatorPositionAnalysis_measureImage""")
  val fileNameList = Seq(
    "TestCollimatorPositionAnalysis_measureImage1.dcm",
    "TestCollimatorPositionAnalysis_measureImage2.dcm")

  val outDir = new File("""target\TestCollimatorPositionAnalysis_measureImage""")
  outDir.mkdirs

  val rtplan = {
    val al = new AttributeList
    al.read(new File(dir, "rtplan.dcm"))
    al
  }

  "TestCollimatorPositionAnalysis_measureImage" should "match expected values" in {

    def doTest(file: File): Unit = {
      println("Processing file " + file.getAbsolutePath)
      val dicomFile = new DicomFile(file)
      val al = dicomFile.attributeList.get
      val dicomImage = new DicomImage(al)
      val biasAndPixelCorrectedCroppedImage = dicomImage
      val pixelCorrectedImage = dicomImage
      val originalImage = dicomImage
      val beamName = file.getName
      val FloodCompensation = false
      val outputPK = -1.toLong
      val collimatorAngle = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head
      println("collimatorAngle: " + collimatorAngle)

      val floodOffset = new Point(0, 0)
      val results = CollimatorPositionAnalysis.testMeasureImage(beamName, FloodCompensation, biasAndPixelCorrectedCroppedImage, pixelCorrectedImage,
        al, originalImage, outputPK, floodOffset, rtplan)

      (results.isRight) should be(true)

      val collimatorPosition = results.right.get._1
      val image = results.right.get._2

      val x1x2y2y2Expected = MeasureTBLREdges.imageCollimatorPositions(al, rtplan)
      println("Expected: " + x1x2y2y2Expected)

      println("collimatorPosition: " + collimatorPosition)

      val imageFile = new File(outDir, file.getName.replace("dcm", "png"))
      imageFile.delete
      println("Writing image file: " + imageFile.getAbsolutePath)
      ImageUtil.writePngFile(image, imageFile)

      (collimatorPosition.status) should be(ProcedureStatus.pass.toString)
    }

    def doTestWithFlood(file: File): Unit = {
      println("Processing file " + file.getAbsolutePath)
      val dicomFile = new DicomFile(file)
      val al = dicomFile.attributeList.get
      val dicomImage = new DicomImage(al)
      val floodOffset = new Point(77, 100)
      val floodRectangle = new Rectangle(floodOffset.x, floodOffset.y, dicomImage.width - (floodOffset.x * 2), dicomImage.height - (floodOffset.y * 2))
      val biasAndPixelCorrectedCroppedImage = dicomImage.getSubimage(floodRectangle)
      val pixelCorrectedImage = dicomImage
      val originalImage = dicomImage
      val beamName = file.getName
      val FloodCompensation = true
      val outputPK = -1.toLong
      val collimatorAngle = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head
      println("collimatorAngle: " + collimatorAngle)

      val results = CollimatorPositionAnalysis.testMeasureImage(beamName, FloodCompensation, biasAndPixelCorrectedCroppedImage, pixelCorrectedImage,
        al, originalImage, outputPK, floodOffset, rtplan)

      (results.isRight) should be(true)

      val collimatorPosition = results.right.get._1
      val image = results.right.get._2

      val x1x2y2y2Expected = MeasureTBLREdges.imageCollimatorPositions(al, rtplan)
      println("Expected: " + x1x2y2y2Expected)

      println("collimatorPosition: " + collimatorPosition)

      val imageFile = new File(outDir, (file.getName + "_flood.png").replace(".dcm", ""))
      imageFile.delete
      println("Writing image file: " + imageFile.getAbsolutePath)
      ImageUtil.writePngFile(image, imageFile)

      (collimatorPosition.status) should be(ProcedureStatus.pass.toString)
    }

    val fileList = dir.listFiles.sortBy(_.getName)
    println("Files to test: " + fileList.map(f => f.getAbsolutePath).mkString("    "))

    fileList.map(f => doTest(f))

    fileList.map(f => doTestWithFlood(f))
    println("TestCollimatorPositionAnalysis_measureImage Done")
  }

}
