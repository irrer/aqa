
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ImageUtil.DicomImage
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Config
import org.scalactic.TolerantNumerics
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis

/**
 * Test Sym+Flat function.
 *
 */

class TestSymmetryAndFlatnessAnalysis extends FlatSpec with Matchers {

  "SymAndFlat" should "show point values" in {

    Trace.trace("Config.validated: " + Config.validated)
    Trace.trace("Starting")

    val dir = new File("""src\test\resources""")
    val file = new File(dir, """SymmetryAndFlatness_J20G0-10F.dcm""")
    Trace.trace("Using file " + file)

    val attributeList = new AttributeList
    attributeList.read(file)
    val dicomImage = new DicomImage(attributeList)

    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head

    val pointSet = SymmetryAndFlatnessAnalysis.testMakePointSet(dicomImage, attributeList, RescaleSlope, RescaleIntercept)

    Trace.trace("values: " + pointSet)

    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.001)

    (pointSet.top === 0.156552) should be(true)
    (pointSet.bottom === 0.155609) should be(true)
    (pointSet.right === 0.156309) should be(true)
    (pointSet.left === 0.156356) should be(true)
    (pointSet.center === 0.224504) should be(true)
    Trace.trace
  }

}