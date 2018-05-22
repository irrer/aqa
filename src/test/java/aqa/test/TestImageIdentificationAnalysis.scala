
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.phase2.PositioningCheckAnalysis

/**
 * Test the PositioningCheckAnalysis.
 */

class TestPositioningCheckAnalysis extends FlatSpec with Matchers {

  "makePositioningCheck" should "make a passing one" in {

    val plan = Util.readDicomFile(new File("""PositioningCheckPlan.dcm""")).right.get
    val image = Util.readDicomFile(new File("""PositioningCheckImage.dcm""")).right.get

    val positioningCheck = PositioningCheckAnalysis.makePositioningCheck(-1, plan, image)
    println("PositioningCheck:\n" + positioningCheck)
    println("Util.buildProperties: " + Util.buildProperties) // TODO rm
    (positioningCheck.get.pass) should be(true)
  }

}
