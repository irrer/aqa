package org.aqa.db

import org.aqa.Logging
import java.io.File
import org.aqa.Config
import scala.xml.XML
import org.aqa.Crypto
import edu.umro.ScalaUtil.Trace
import scala.collection.mutable.ArrayBuffer
import org.aqa.Util
import org.aqa.web.WebUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.AnonymizeUtil
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TagFromName

/** Transition to an anonymized database. */

object DbTransitionToAnonymize extends Logging {

  private def fmt(maxLen: Int, any: Any) = {
    val txt = any.toString
    val t = if (txt.size > (maxLen - 3)) txt.take(maxLen - 3) + "..." else txt.take(maxLen)
    t.formatted("%-" + maxLen + "s")
  }

  private def fmt20(any: Any) = fmt(20, any)
  private def fmt12(any: Any) = fmt(12, any)

  private def anonymizeInstitutions = {

    def anon(institution: Institution) = {
      if (institution.name_real.isEmpty) {
        val pk = institution.institutionPK.get
        def encrypt(text: String): String = AnonymizeUtil.encryptWithNonce(pk, text)

        val newInst = new Institution(
          Some(pk),
          AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasPrefixId, institution.institutionPK.get),
          Some(encrypt(institution.name)),
          encrypt(institution.url_real),
          encrypt(institution.description_real))

        def show(inst: Institution) = {
          "    name: " + fmt12(inst.name) +
            "    name_real: " + fmt20(inst.name_real) +
            "    url_real: " + fmt20(inst.url_real) +
            "    description_real: " + fmt20(inst.description_real)
        }

        val both = show(institution) + " ==>\n" + show(newInst)

        logger.info("ConvertToAnonymousDatabase Need to convert institution from :\n" + both)
        if (Config.ConvertToAnonymousDatabase) {
          newInst.insertOrUpdate
          logger.info("ConvertToAnonymousDatabase Updating institution from :\n" + both)
        }
      }
    }

    Institution.list.map(inst => anon(inst))
  }

  /**
   * Anonymize and insert the given device serial number attribute into the database and return the alias value.
   */
  def insertDeviceSerialNumber(institutionPK: Long, deviceSerialNumber: String): String = {

    // determine if it is already in the database.
    val attr = AttributeFactory.newAttribute(TagFromName.DeviceSerialNumber)
    attr.addValue(deviceSerialNumber)
    val daList = DicomAnonymous.getAttributes(institutionPK, Seq(attr))

    // If it is already in the database, then do nothing.
    if (daList.nonEmpty)
      daList.head.value // already in database.
    else {
      logger.info("ConvertToAnonymousDatabase Need to insert DeviceSerialNumber attribute for: " + deviceSerialNumber)
      if (Config.ConvertToAnonymousDatabase) {
        val da = DicomAnonymous.insert(institutionPK, attr)
        logger.info("ConvertToAnonymousDatabase Inserting DeviceSerialNumber attribute for: " + deviceSerialNumber)
        da.value
      } else {
        "notAnonYet"
      }
    }
  }

  private def anonymizeMachines = {

    def anon(machine: Machine) = {
      val aliasId = AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasPrefixId, machine.machinePK.get)
      val id_real = Some(AnonymizeUtil.encryptWithNonce(machine.institutionPK, machine.id))
      val notes_encrypted = AnonymizeUtil.encryptWithNonce(machine.institutionPK, machine.notes)

      def anonId(m: Machine) = {

        val anonSerNo: Option[String] = if (machine.serialNumber.isDefined) {
          val anonSer = insertDeviceSerialNumber(machine.institutionPK, machine.serialNumber.get)
          Some(anonSer)
        } else None

        m.copy(id_real = id_real).copy(id = aliasId, serialNumber = anonSerNo, notes = notes_encrypted)
      }

      def update(m: Machine) = {
        def show(mch: Machine) = {
          "    id: " + fmt12(mch.id) +
            "    mch.id_real: " + fmt20(mch.id_real) +
            "    mch.serialNumber: " + fmt20(mch.serialNumber) +
            "    mch.notes: " + fmt20(mch.notes)
        }
        val both = show(machine) + " ==>\n" + show(m)
        logger.info("ConvertToAnonymousDatabase Need to convert machine from :\n" + both)

        if (Config.ConvertToAnonymousDatabase) {
          m.insertOrUpdate
          logger.info("ConvertToAnonymousDatabase Updating machine from :\n" + both)
        }
      }

      if (machine.id_real.isEmpty) update(anonId(machine))
    }

    Machine.list.map(m => anon(m))
  }

  private def anonymizeUsers = {

    def anon(user: User) = {
      if (user.id_real.isEmpty) {
        def encrypt(text: String): String = AnonymizeUtil.encryptWithNonce(user.institutionPK, text)

        val newUser = user.copy(
          id = AnonymizeUtil.aliasify(AnonymizeUtil.userAliasPrefixId, user.userPK.get),
          id_real = Some(encrypt(user.id)),
          fullName_real = encrypt(user.fullName_real),
          email_real = encrypt(user.email_real))

        def show(user: User) = {
          "    id: " + fmt12(user.id) +
            "    id_real: " + fmt20(user.id_real) +
            "    url_real: " + fmt20(user.fullName_real) +
            "    description_real: " + fmt20(user.email_real)
        }

        val both = show(user) + " ==>\n" + show(newUser)

        logger.info("ConvertToAnonymousDatabase Need to convert user from :\n" + both)
        if (Config.ConvertToAnonymousDatabase) {
          newUser.insertOrUpdate
          logger.info("ConvertToAnonymousDatabase Updating user from :\n" + both)
        }
      }
    }

    User.list.map(user => anon(user))
  }

  def transition = {
    anonymizeInstitutions
    anonymizeMachines
    anonymizeUsers
    logger.info("finished transition of anonymization security")
  }

  def fixMachineNotes = {
    def fixMachNotes(machine: Machine) = {

      def show(mch: Machine) = {
        "    id: " + fmt12(mch.id) +
          "    mch.id_real: " + fmt20(mch.id_real) +
          "    mch.serialNumber: " + fmt20(mch.serialNumber) +
          "    mch.notes: " + fmt20(mch.notes)
      }

      /**
       * Test to see if the field needs encrypting by making sure that it is not already
       * encrypted.  If it was encrypted, then it is long enough and all hex.
       */
      val needsEncrypting: Boolean = {
        try {
          if (machine.notes.size < 64) throw new RuntimeException
          Crypto.hexToByteArray(machine.notes)
          false
        } catch {
          case t: Throwable => true
        }
      }

      if (needsEncrypting) {
        val notes_encrypted = AnonymizeUtil.encryptWithNonce(machine.institutionPK, machine.notes)
        val fixed = machine.copy(notes = notes_encrypted)
        logger.info("would fix mach note from\n    " + show(machine) + " ==>\n    " + show(fixed))

        if (Config.ConvertToAnonymousDatabase) {
          fixed.insertOrUpdate
        }
      }
    }
    Machine.list.map(m => fixMachNotes(m))
  }
}
