
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.web.C3Chart
import java.awt.Color
import org.aqa.Crypto
import org.aqa.webrun.phase2.cbctAlign.CBCTAnalysis
import org.aqa.DicomFile
import org.aqa.Config

/**
 * Test the Config.
 *
 */

class TestCBCTAnalysis extends FlatSpec with Matchers {

  Config.validate
  println("-----------------------------------------------------------------------------------------------------")

  // list of directories that contain a CBCT set for analysis
  val dirList = (new File("""src\test\resources\TestCBCTAlign""")).listFiles
  println("List of CBCT directories used as input:\n    " + dirList.map(dir => dir.getAbsolutePath).mkString("\n    "))

  "TestCBCTAnalysis" should "find BB" in {

    def testDir(dir: File) = {
      println("Processing directory " + dir.getAbsolutePath)
      val attrListSeq = dir.listFiles.map(f => (new DicomFile(f)).attributeList.get).toSeq
      println("Number of slices in series: " + attrListSeq.size)
      val result = CBCTAnalysis.testAnalyze(attrListSeq)
      Trace.trace(result)
      (result.isRight) should be(true) // Expected voxel coordinates: 246, 262, 42
    }

    Trace.trace
    dirList.map(dir => testDir(dir))
    Trace.trace

    (11 > 10) should be(true)
  }

}
