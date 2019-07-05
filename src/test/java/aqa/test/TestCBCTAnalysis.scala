
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
import org.aqa.webrun.phase2.centerDose.CBCTAnalysis
import org.aqa.DicomFile
import org.aqa.Config

/**
 * Test the Config.
 *
 */

class TestCBCTAnalysis extends FlatSpec with Matchers {

  Config.validate
  
  // list of directories that contain a CBCT set for analysis
  val dirList = (new File("""src\test\resources\TestCBCTAlign""")).listFiles

  "TestCBCTAnalysis" should "find BB" in {

    def testDir(dir: File) = {
      val attrListSeq = dir.listFiles.map(f => (new DicomFile(f)).attributeList.get).toSeq
      val result = CBCTAnalysis.testAnalyze(attrListSeq)
      (result.isRight) should be(true)
    }

    dirList.map(dir => testDir(dir))

    (11 > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Crypto.randomSecureHash).distinct
    list.size should be(size)
  }
}
