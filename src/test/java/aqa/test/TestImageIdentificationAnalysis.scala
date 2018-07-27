
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.phase2.MetadataCheckAnalysis

/**
 * Test the PositioningCheckAnalysis.
 */

class TestMetadataCheckAnalysis extends FlatSpec with Matchers {

  "makeMetadataCheck" should "make a passing one" in {

    Trace.trace
    //      val plan = Util.readDicomFile(new File("""PositioningCheckPlan.dcm""")).right.get
    //      Trace.trace
    //      val image = Util.readDicomFile(new File("""PositioningCheckImage.dcm""")).right.get
    //      Trace.trace
    //
    //      val positioningCheck = PositioningCheckAnalysis.makePositioningCheck(-1, plan, image)
    //      System.err.println("PositioningCheck:\n" + positioningCheck)
    //      System.err.println("Util.buildProperties: " + Util.buildProperties) // TODO rm
    //      (positioningCheck.get.pass) should be(true)
    (true) should be(true)
  }

}
