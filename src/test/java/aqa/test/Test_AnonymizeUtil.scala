
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Crypto
import scala.util.Random
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.db.DbSetup
import org.aqa.db.DicomAnonymous

/**
 * Test AnonymizeUtil.
 *
 */

class Test_AnonymizeUtil extends FlatSpec with Matchers {

  Config.validate
  DbSetup.init
  val original = new AttributeList
  original.read(new File("""src\test\resources\AnonymizeDicom.dcm"""))
  val originalAsText = DicomUtil.attributeListToString(original) // for debugging

  val institutionPK = 2.toLong

  def areEq(a: AttributeList, b: AttributeList, tag: AttributeTag): Boolean = {
    a.get(tag).getSingleStringValueOrEmptyString.equals(b.get(tag).getSingleStringValueOrEmptyString)
  }

  def getAllDa = DicomAnonymous.listDicomAnonymousFromInstitution(institutionPK).map(da => da.dicomAnonymousPK.get)

  val preExistingList = getAllDa

  "standard anonymize" should "be anonymized" in {

    val anonymized1 = AnonymizeUtil.anonymizeDicom(institutionPK, original)
    val anonymized2 = AnonymizeUtil.anonymizeDicom(institutionPK, original)
    def get1(tag: AttributeTag): String = anonymized1.get(tag).getSingleStringValueOrEmptyString
    def get2(tag: AttributeTag): String = anonymized2.get(tag).getSingleStringValueOrEmptyString

    val anonymizedAsText1 = DicomUtil.attributeListToString(anonymized1)
    println("\n\nBefore: " + originalAsText)
    println("\n\n\nAfter: " + anonymizedAsText1)

    areEq(original, anonymized1, TagFromName.SOPInstanceUID) should be(false)
    areEq(anonymized1, anonymized1, TagFromName.SOPInstanceUID) should be(true)
    areEq(anonymized1, anonymized2, TagFromName.SOPInstanceUID) should be(true)
    areEq(original, anonymized1, TagFromName.PatientID) should be(false)
    areEq(anonymized1, anonymized2, TagFromName.PatientID) should be(true)

    val newList = getAllDa

    val newDa = newList.diff(preExistingList)
    println("new DicomAnonymous: " + newDa.mkString("  "))
    println("removing " + newDa.size + " DicomAnonymous rows made during testing...")
    newDa.map(daPk => DicomAnonymous.delete(daPk))

  }

}
