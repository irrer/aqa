
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import org.aqa.DicomFile
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil

/**
 * Test TestLeafPositionUtil.
 */

class TestLeafPositionUtil extends FlatSpec with Matchers {

  private val dir = new File("""src\test\resources\TestLeafPositionUtil""")
  private val beamName = "PF Stat 180"

  val valid = Seq(
    Seq(-90.0, -85.0, -80.0, -75.0, -70.0, -65.0, -60.0, -55.0, -50.0, -45.0, -40.0, -37.5, -35.0, -32.5,
      -30.0, -27.5, -25.0, -22.5, -20.0, -17.5, -15.0, -12.5, -10.0, -7.5, -5.0, -2.5, 0.0, 2.5, 5.0,
      7.5, 10.0, 12.5, 15.0, 17.5, 20.0, 22.5, 25.0, 27.5, 30.0, 32.5, 35.0, 37.5, 40.0, 45.0, 50.0,
      55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0),
    Seq(-90.0, -85.0, -80.0, -75.0, -70.0, -65.0, -60.0, -55.0, -50.0, -45.0, -40.0, -35.0, -30.0, -25.0,
      -20.0, -15.0, -10.0, -5.0, 0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,
      60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0))

  private def testPlan(file: File) = {
    val plan = (new DicomFile(file)).attributeList.get

    val leafEdgeList = LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(true, beamName, plan)

    println("leafEdgeList size: " + leafEdgeList.size)
    println("leafEdgeList: " + leafEdgeList.map(le => Util.fmtDbl(le)).mkString("  "))

    (valid.contains(leafEdgeList)) should be(true)
  }

  "TestLeafPositionUtil" should "find leaf sides" in {
    println("Starting...")

    val fileList = dir.listFiles.toList

    fileList.map(file => testPlan(file))

    (true) should be(true)
  }

}
