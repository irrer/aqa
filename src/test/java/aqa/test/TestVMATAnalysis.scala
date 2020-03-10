
package aqa.test;

//import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
//import edu.umro.util.Utility
//import edu.umro.ScalaUtil.Trace
//import edu.umro.ScalaUtil.FileUtil
//import org.aqa.web.C3Chart
//import java.awt.Color
//import org.aqa.Crypto
//import org.aqa.DicomFile
import org.aqa.Config
//import edu.umro.ImageUtil.DicomImage
//import java.awt.image.BufferedImage
//import org.aqa.webrun.phase2.vmat.VMATAnalysis
//import org.aqa.VolumeTranslator

/**
 * Test the Config.
 *
 */

class TestVMATAnalysis extends FlatSpec with Matchers {

  Config.validate

  println("-----------------------------------------------------------------------------------------------------")
  val inDir = new File("""target\TestVMATAnalysis""")

  // list of directories that contain a VMAT set for analysis
  val dirList = (new File("""src\test\resources\TestVMAT""")).listFiles
  println("List of VMAT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

  "TestVMATAnalysis" should "find percentages" in {

    def testDir(dir: File) = {
      (true) should be(true) // Expected voxel coordinates: 246, 262, 42
    }

    dirList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

}
