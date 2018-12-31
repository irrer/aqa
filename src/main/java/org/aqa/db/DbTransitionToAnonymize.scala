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
        def encrypt(text: String): String = AnonymizeUtil.encrypt(pk, text)

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

  private def anonymizeMachines = {

    def anon(machine: Machine) = {
      val aliasId = AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasPrefixId, machine.machinePK.get)
      val aliasSerNum = Some(AnonymizeUtil.aliasify(AnonymizeUtil.machineAliasPrefixSerialNumber, machine.machinePK.get))
      def id_real = Some(AnonymizeUtil.encrypt(machine.institutionPK, machine.id))
      def serNum_real = Some(AnonymizeUtil.encrypt(machine.institutionPK, machine.serialNumber.get))

      def anonId(m: Machine) = m.copy(id_real = id_real).copy(id = aliasId)

      def anonSerialNum(m: Machine) = m.copy(serialNumber = aliasSerNum).copy(serialNumber_real = serNum_real)

      val encryptId = machine.id_real.isEmpty

      val encryptSerNo = machine.serialNumber.isDefined && machine.serialNumber_real.isEmpty

      def update(m: Machine) = {
        def show(mch: Machine) = {
          "    id: " + fmt12(mch.id) +
            "    mch.id_real: " + fmt20(mch.id_real) +
            "    mch.serialNumber: " + fmt20(mch.serialNumber) +
            "    mch.serialNumber_real: " + fmt20(mch.serialNumber_real)
        }
        val both = show(machine) + " ==>\n" + show(m)
        logger.info("ConvertToAnonymousDatabase Need to convert machine from :\n" + both)
        if (Config.ConvertToAnonymousDatabase) {
          m.insertOrUpdate
          logger.info("ConvertToAnonymousDatabase Updating machine from :\n" + both)
        }
      }

      (encryptId, encryptSerNo) match {
        case (true, true) => update(anonId(anonSerialNum(machine)))
        case (true, false) => update(anonId(machine))
        case (false, true) => update(anonSerialNum(machine))
        case (false, false) => ; // nothing needs changing
      }
    }

    Machine.list.map(m => anon(m))
  }

  private def anonymizeUsers = {

    def anon(user: User) = {
      if (user.id_real.isEmpty) {
        def encrypt(text: String): String = AnonymizeUtil.encrypt(user.institutionPK, text)

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
}
