package aqa.test

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.db.DbSetup
import org.aqa.db.DicomAnonymous
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test AnonymizeUtil.
  *
  */

class Test_AnonymizeUtil extends FlatSpec with Matchers {

  Config.validate
  DbSetup.init
  println("Starting test.")
  if (false) {

    if (true) {
      val instList = Institution.list

      val machList = Machine.list

      for (inst <- instList; mach <- machList) {
        val id = AnonymizeUtil.decryptWithNonce(inst.institutionPK.get, mach.id_real.get)
        if (!id.equals("Could not decrypt")) {
          println(
            "decrypted machine " + mach.machinePK.get +
              " : " + mach.id +
              "  machInst: " + mach.institutionPK +
              " with inst " + inst.name +
              " to " + id +
              "    mach notes " + AnonymizeUtil.decryptWithNonce(inst.institutionPK.get, mach.notes)
          )
        }
      }
    }

    for (machPK <- Seq(14, 16, 18, 19)) {
      println("Fixing machine " + machPK)
      val machine = Machine.get(machPK).get
      val id_r = AnonymizeUtil.aliasify("TXM_", machine.machinePK.get)

      val id_enc = AnonymizeUtil.encryptWithNonce(machine.institutionPK, id_r)
      val note_enc = AnonymizeUtil.encryptWithNonce(machine.institutionPK, "")
      val mach2 = machine.copy(id_real = Some(id_enc), notes = note_enc)
      mach2.insertOrUpdate
    }

    System.exit(99)
  }
  private val original = new AttributeList

  private def dicomToText(al: AttributeList): String = {
    val text = DicomUtil.attributeListToString(al).replaceAll("<null>", "").replaceAll("  *\n", "\n")
    text
  }

  original.read(new File("""D:\tmp\aqa\CBCT\MQATX2OBIQA2019Q3\CT.MQATX2OBIQA2019Q3.Image_32_0012.dcm"""))
  // original.read(new File("""D:\tmp\aqa\CBCT\MQATX2OBIQA2019Q3\CT.MQATX2OBIQA2019Q3.Image_57_0027.dcm"""))
  // original.read(new File("""src\test\resources\AnonymizeDicom.dcm"""))
  private val originalAsText = dicomToText(original) // for debugging

  private val institutionPK = 2.toLong

  private def areEq(a: AttributeList, b: AttributeList, tag: AttributeTag): Boolean = {
    a.get(tag).getSingleStringValueOrEmptyString.equals(b.get(tag).getSingleStringValueOrEmptyString)
  }

  private val lastPK = DicomAnonymous.getLastPk()

  "standard anonymize" should "be anonymized" in {

    val anonymized1 = AnonymizeUtil.anonymizeDicom(institutionPK, original)
    val anonymized2 = AnonymizeUtil.anonymizeDicom(institutionPK, original)

    val anonymizedAsText1 = dicomToText(anonymized1)
    println("\n\nBefore:\n" + originalAsText)
    println("\n\n\nAfter:\n" + anonymizedAsText1)

    areEq(original, anonymized1, TagFromName.SOPInstanceUID) should be(false)
    areEq(anonymized1, anonymized1, TagFromName.SOPInstanceUID) should be(true)
    areEq(anonymized1, anonymized2, TagFromName.SOPInstanceUID) should be(true)
    areEq(original, anonymized1, TagFromName.PatientID) should be(false)
    areEq(anonymized1, anonymized2, TagFromName.PatientID) should be(true)

    // ----------------------------------------------------------------------------------------

    val roundTrip = AnonymizeUtil.deAnonymizeDicom(institutionPK, Seq(anonymized1))
    val roundTripText = dicomToText(roundTrip.head)
    println("roundTripText\n" + roundTripText)
    roundTripText.equals(originalAsText) should be(true)

    // ----------------------------------------------------------------------------------------

    // remove DicomAnonymous entries that were created by this test.
    val newDaList = DicomAnonymous.getByPkLargerThan(lastPK)
    println("number to be deleted: " + newDaList.size)
    println("List of new DicomAnonymous rows to be deleted:\n" + newDaList.mkString("\n"))
    newDaList.map(da => DicomAnonymous.delete(da.dicomAnonymousPK.get))
    println("done")

  }

}
