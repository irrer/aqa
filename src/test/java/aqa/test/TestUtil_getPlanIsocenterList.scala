
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import edu.umro.ScalaUtil.Trace
import java.io.File
import org.aqa.DicomFile
import com.pixelmed.dicom.AttributeList

/**
 * Test the Util.getPlanIsocenterList
 */

class TestUtil_getPlanIsocenterList extends FlatSpec with Matchers {

  val dir = new File("""src\test\resources\TestUtil_getPlanIsocenterList""")
  println("Using input directory " + dir.getAbsolutePath)

  val rtplanFileList = dir.listFiles.map(f => (new DicomFile(f))).toSeq // (new DicomFile(f)).attributeList.get)

  "getPlanIsocenterList" should "find isocenters defined in plan" in {

    def testIt(rtplan: DicomFile) {
      val isocenterList = Util.getPlanIsocenterList(rtplan.attributeList.get)

      println("Plan file: " + rtplan.file.getAbsolutePath)
      println("Isocenter list:\n    " + isocenterList.map(p => Util.fmtDbl(p.getX) + ", " + Util.fmtDbl(p.getY) + ", " + Util.fmtDbl(p.getZ)).mkString("\n    "))
      println("Distinct list:\n    " + isocenterList.distinct.map(p => Util.fmtDbl(p.getX) + ", " + Util.fmtDbl(p.getY) + ", " + Util.fmtDbl(p.getZ)).mkString("\n    "))
      true should be(true)

    }

    rtplanFileList.map(rtplan => testIt(rtplan))
  }
}
