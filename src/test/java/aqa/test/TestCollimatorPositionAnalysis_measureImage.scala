
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

/**
 * Test the Config.
 *
 */

class TestCollimatorPositionAnalysis_measureImage extends FlatSpec with Matchers {

  Config.validate

  val dir = new File("""src\test\resources""")
  val fileNameList = Seq("TestCollimatorPositionAnalysis_measureImage1.dcm", "TestCollimatorPositionAnalysis_measureImage2.dcm")

  val outDir = new File("""target\TestCollimatorPositionAnalysis_measureImage""")
  outDir.mkdirs

  "TestCollimatorPositionAnalysis_measureImage" should "match expected values" in {

    def doTest(fileName: String) = {
      val file = new File(dir, fileName)
      println("Processing file " + file.getAbsolutePath)
      val dicomFile = new DicomFile(file)
      val al = dicomFile.attributeList.get
      val dicomImage = new DicomImage(al)
      val biasAndPixelCorrectedCroppedImage = dicomImage
      val pixelCorrectedImage = dicomImage
      val originalImage = dicomImage
      val beamName = fileName
      val FloodCompensation = false
      val outputPK = -1.toLong
      val floodOffset = new Point(0, 0)
      val results = CollimatorPositionAnalysis.testMeasureImage(beamName, FloodCompensation, biasAndPixelCorrectedCroppedImage, pixelCorrectedImage,
        al, originalImage, outputPK, floodOffset)

      (results.isRight) should be(true)

      val collimatorPosition = results.right.get._1
      val image = results.right.get._2

      println("collimatorPosition: " + collimatorPosition)

      val imageFile = new File(outDir, fileName.replace("dcm", "png"))
      println("Writing image file: " + imageFile.getAbsolutePath)
      ImageUtil.writePngFile(image, imageFile)

      (collimatorPosition.X1_ExpectedMinusImage_mm.abs < 2) should be(true)
      (collimatorPosition.X2_ExpectedMinusImage_mm.abs < 2) should be(true)
      (collimatorPosition.Y1_ExpectedMinusImage_mm.abs < 2) should be(true)
      (collimatorPosition.Y2_ExpectedMinusImage_mm.abs < 2) should be(true)

    }

    fileNameList.map(fn => doTest(fn))
  }
}
