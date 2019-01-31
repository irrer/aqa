
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

/**
 * Test the Config.
 *
 */

class TestCollimatorPositionAnalysis_measureImage extends FlatSpec with Matchers {

  "TestCollimatorPositionAnalysis_measureImage" should "match expected values" in {

    val file = new File("""src\test\resources\TestCollimatorPositionAnalysis_measureImage.dcm""")
    val dicomFile = new DicomFile(file)
    val al = dicomFile.attributeList.get
    val dicomImage = new DicomImage(al)
    val biasAndPixelCorrectedCroppedImage = dicomImage
    val pixelCorrectedImage = dicomImage
    val originalImage = dicomImage
    val beamName = "J10G180"
    val FloodCompensation = false
    val outputPK = -1.toLong
    val floodOffset = new Point(0, 0)
    val results = CollimatorPositionAnalysis.measureImage(beamName, FloodCompensation, biasAndPixelCorrectedCroppedImage, pixelCorrectedImage,
      al, originalImage, outputPK, floodOffset)

    (results.isRight) should be(true)

    val collimatorPosition = results.right.get._1
    val image = results.right.get._2

    println("collimatorPosition: " + collimatorPosition)

    val outDir = new File("target")
    val imageFile = new File(outDir, "TestCollimatorPositionAnalysis_measureImage.png")
    println("Writing image file: " + imageFile.getAbsolutePath)
    ImageUtil.writePngFile(image, imageFile)

    (collimatorPosition.X1_ExpectedMinusImage_mm.abs < 2) should be (true)
    (collimatorPosition.X2_ExpectedMinusImage_mm.abs < 2) should be (true)
    (collimatorPosition.Y1_ExpectedMinusImage_mm.abs < 2) should be (true)
    (collimatorPosition.Y2_ExpectedMinusImage_mm.abs < 2) should be (true)
    
  }

}
