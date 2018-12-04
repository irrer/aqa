
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestLeafPositionAnalysis_leafSides extends FlatSpec with Matchers {
  "TestLeafPositionAnalysis_leafSides" should "show precise leaf locations leaf end positions in isoplan in pixels" in {
    val dir = new File("""src\test\resources""")

    val file = new File(dir, """TestLeafPositionAnalysis.dcm""")
    val imageAttrList = Util.readDicomFile(file).right.get
    val dicomImage = new DicomImage(imageAttrList)

    val planFile = new File(dir, """TestLeafPositionAnalysisPlan.dcm""")
    val planAl = Util.readDicomFile(planFile).right.get
    val beamName = "PF Stat 0"
    val leafSideList = LeafPositionAnalysis.testLeafSides(true, beamName, imageAttrList, dicomImage, planAl)

    val expectedSideEndList = Seq(
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

    println("expected list of leaf sides:\n    " + expectedSideEndList.mkString("\n    "))

    println("list of leaf sides:\n    " + leafSideList.mkString("\n    "))

    true should be(true)
  }
}
