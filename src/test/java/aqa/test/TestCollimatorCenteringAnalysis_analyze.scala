
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
import org.aqa.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil
import org.aqa.webrun.phase2.centerDose.CenterDoseAnalysis
import org.aqa.DicomFile
import org.aqa.db.CollimatorCentering
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringAnalysis
import org.aqa.webrun.phase2.Phase2Util
import java.awt.image.BufferedImage

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestCollimatorCenteringAnalysis_analyze extends FlatSpec with Matchers {

  Config.validate

  val dir = new File("""src\test\resources""")
  val fileNameList = Seq(
    ("TestCollimatorCentering090a.dcm", "TestCollimatorCentering270a.dcm"),
    ("TestCollimatorCentering090b.dcm", "TestCollimatorCentering270b.dcm"))

  val outDir = new File("""target\TestCollimatorCenteringAnalysis""")
  outDir.mkdirs

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
      val radius = (translator.iso2PixDistX(Config.BadPixelRadius_mm)).round.toInt
      val imageOrig = new DicomImage(dicomFile.attributeList.get)
      val image = imageOrig.correctBadPixels(Phase2Util.identifyBadPixels(imageOrig, radius), radius)
      image
    }

    def testFilePair(fileName090: String, fileName270: String) = {
      val dicomFile090 = new DicomFile(new File(dir, fileName090))
      val dicomFile270 = new DicomFile(new File(dir, fileName270))
      println("\n\nfiles:\n    " + dicomFile090.file.getAbsolutePath + "\n    " + dicomFile270.file.getAbsolutePath)

      val image090 = correctBadPix(dicomFile090)
      val image270 = correctBadPix(dicomFile270)

      val analysisResult = CollimatorCenteringAnalysis.testAnalyze(dicomFile090, dicomFile270, image090, image270, -1)

      val collimatorCentering = analysisResult._1
      val bufImage090 = analysisResult._2
      val bufImage270 = analysisResult._3

      writePng(fileName090, bufImage090.bufferedImage)
      writePng(fileName270, bufImage270.bufferedImage)

      println("centerDose: " + collimatorCentering)
    }

    fileNameList.map(fn => testFilePair(fn._1, fn._2))

    true should be(true)
  }
}
