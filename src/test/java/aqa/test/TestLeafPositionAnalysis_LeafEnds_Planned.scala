
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestLeafPositionAnalysis_LeafEnds_Planned extends FlatSpec with Matchers {
  "LeafPositionAnalysis" should "show leaf end positions in isoplan in mm" in {
    val dir = new File("""src\test\resources""")

    val planFile = new File(dir, """TestLeafPositionAnalysisPlan.dcm""")
    val planAl = Util.readDicomFile(planFile).right.get
    val leafEndList = LeafPositionUtil.leafEnds(true, "PF Stat 0", planAl)

    val expectedLeafEndList = Seq(
      -60.0,
      -45.0,
      -30.0,
      -15.0,
      0.0,
      15.0,
      30.0,
      45.0,
      60.0,
      75.0)

    println("list of leaf ends:\n    " + leafEndList.mkString("\n    "))

    leafEndList should be(expectedLeafEndList)
  }
}
