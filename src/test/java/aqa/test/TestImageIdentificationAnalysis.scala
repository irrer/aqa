
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.ImageIdentificationAnalysis

/**
 * Test the ImageIdentificationAnalysis.
 */

class TestImageIdentificationAnalysis extends FlatSpec with Matchers {

  "makeImageIdentification" should "make a passing one" in {

    val plan = Util.readDicomFile(new File("""ImageIdentificationPlan.dcm""")).right.get
    val image = Util.readDicomFile(new File("""ImageIdentificationImage.dcm""")).right.get

    val imageIdentification = ImageIdentificationAnalysis.makeImageIdentification(plan, image)
    println("ImageIdentification:\n" + imageIdentification)
    println("Util.buildProperties: " + Util.buildProperties) // TODO rm
    (imageIdentification.get.pass) should be(true)
  }

}
