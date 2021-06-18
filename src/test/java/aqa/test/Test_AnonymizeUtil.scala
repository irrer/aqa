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
  val original = new AttributeList
  val file = new File("""src\test\resources\AnonymizeDicom.dcm""")
  println("Reading DICOM file " + file.getAbsolutePath)
  original.read(file)
  val originalAsText: String = DicomUtil.attributeListToString(original) // for debugging

  val institutionPK: Long = 2.toLong

  def areEq(a: AttributeList, b: AttributeList, tag: AttributeTag): Boolean = {
    a.get(tag).getSingleStringValueOrEmptyString.equals(b.get(tag).getSingleStringValueOrEmptyString)
  }

  def getAllDa: Seq[Long] = DicomAnonymous.listDicomAnonymousFromInstitution(institutionPK).map(da => da.dicomAnonymousPK.get)

  val preExistingList: Seq[Long] = getAllDa

  "standard anonymize" should "be anonymized" in {

    val anonymized1 = AnonymizeUtil.anonymizeDicom(institutionPK, original)
    val anonymized2 = AnonymizeUtil.anonymizeDicom(institutionPK, original)

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

    val roundTrip = AnonymizeUtil.deAnonymizeDicom(institutionPK, Seq(anonymized1)).head

    val roundTripAsText = DicomUtil.attributeListToString(roundTrip) // for debugging
    println("roundTripAsText\n" + roundTripAsText)

    roundTripAsText.equals(originalAsText) should be(true)
  }

}
