
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.db.DbSetup
import org.aqa.db.DbTransitionToAnonymize
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TagFromName
import org.aqa.db.DicomAnonymous
import org.aqa.db.Institution
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.Attribute

/**
 * Test DbAnonymize.
 *
 */

class Test_DicomAnonymous extends FlatSpec with Matchers {

  println("Starting...")
  Config.validate
  DbSetup.init

  val dir = new File("""src\test\resources""")

  val dicomFile = new File(dir, "Test_DicomAnonymous.dcm")
  val al = Util.readDicomFile(dicomFile).right.get

  "getAttributes" should "be getting attributes" in {

    val inst = (0 until 4).map(i => Institution.get(i)).flatten.head

    val attr1 = al.get(TagFromName.PatientID)
    val attr2 = al.get(TagFromName.SOPInstanceUID)
    val attr3 = al.get(TagFromName.DeviceSerialNumber)

    val attrList = Seq(attr1, attr2, attr3)

    def doit(at: Attribute) = {
      val after = DicomAnonymous.insert(inst.institutionPK.get, at)
      val actualAfter = DicomAnonymous.get(after.dicomAnonymousPK.get).get
      val text = "\n\nbefore\n" + at +
        "\nafter:\n" + after.attributeTag + " : " + after.value +
        "\nactualAfter:\n" + actualAfter.attributeTag + " : " + actualAfter.value
      println(text.replace('\0', ' '))

      DicomAnonymous.delete(after.dicomAnonymousPK.get)
    }

    attrList.map(at => doit(at))

    println("\n")
    true should be(true)
  }
  println("Done")
}
