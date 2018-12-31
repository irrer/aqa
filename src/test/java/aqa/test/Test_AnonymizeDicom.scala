
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
import org.aqa.AnonymizeDicom
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName

/**
 * Test the Config.
 *
 */

class Test_AnonymizeDicom extends FlatSpec with Matchers {

  val source = new AttributeList
  source.read(new File("""src\test\resources\AnonymizeDicom.dcm"""))
  val sourceAsText = DicomUtil.attributeListToString(source) // for debugging

  "anon with no changes" should "be the same" in {

    val uidHistory = Map[String, String]()
    val history = Map[AttributeTag, Map[String, String]]()
    val phi = Map[AttributeTag, String]()

    val anonymizeDicom = new AnonymizeDicom(history, phi)

    val dest = anonymizeDicom.anonymize(source)
    val destAsText = DicomUtil.attributeListToString(dest) // for debugging

    sourceAsText.equals(destAsText) should be(true)
  }

  "anon" should "change some fields" in {

    val newFrameOfRef = "12345.FrameOfReferenceUID"
    val uidHistory = Map[String, String](("1.2.246.352.62.3.5253998828285979696.5369156955102472089", newFrameOfRef))

    val PatientID = (TagFromName.PatientID, Map(("$JM_AQA_phase2_v000", "PAT___5"), ("bert", "ernie")))
    val PatientName = (TagFromName.PatientName, Map(("$JM_AQA_phase2_v000", "PAT___5"), ("bert", "ernie")))
    val FrameOfReferenceUID = (TagFromName.FrameOfReferenceUID, Map(("1.2.246.352.62.3.5253998828285979696.5369156955102472089", "12345.FrameOfReferenceUID")))
    val history = Map(PatientID, PatientName, FrameOfReferenceUID)

    val phi = Map[AttributeTag, String](
      (TagFromName.ReferringPhysicianName, "big Al"))

    val anonymizeDicom = new AnonymizeDicom(history, phi)

    val dest = anonymizeDicom.anonymize(source)
    val destAsText = DicomUtil.attributeListToString(dest) // for debugging

    def checkTag(tag: AttributeTag, value: String): Boolean = dest.get(tag).getSingleStringValueOrEmptyString.equals(value)

    checkTag(TagFromName.FrameOfReferenceUID, newFrameOfRef) should be(true)
    checkTag(TagFromName.PatientID, "PAT___5") should be(true)
    checkTag(TagFromName.PatientName, "PAT___5") should be(true)
    checkTag(TagFromName.PatientName, "ernie") should be(false)
    checkTag(TagFromName.ReferringPhysicianName, "big Al") should be(true)
    checkTag(TagFromName.StationName, "UM_TB1") should be(true)

    sourceAsText.equals(destAsText) should be(false)
  }

}
