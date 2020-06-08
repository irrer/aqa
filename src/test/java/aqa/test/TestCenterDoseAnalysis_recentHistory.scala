
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

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestCenterDoseAnalysis_testConstructCenterDose extends FlatSpec with Matchers {

  val dir = new File("""src\test\resources""")
  val fileNameList = Seq(
    "TestCenterDoseFlood.dcm",
    "TestCenterDoseJ20G0_6F.dcm",
    "TestCenterDoseT3Open.dcm")

  "TestCenterDoseAnalysis_testConstructCenterDose" should "yield a center dose" in {

    def testFile(fileName: String) = {
      val dicomFile = new DicomFile(new File(dir, fileName))
      println("\n\nfile: " + dicomFile.file.getAbsolutePath)
      val beamName = fileName
      val centerDose = CenterDoseAnalysis.testConstructCenterDose(beamName, dicomFile)
      println("centerDose: " + centerDose)
      System.exit(99)
    }

    fileNameList.map(fn => testFile(fn))

    true should be(true)
  }
}
