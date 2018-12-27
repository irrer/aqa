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

/** Transition to an anonymized database. */

object DbTransitionToAnonymize extends Logging {

  private def anonymizeMachines = {
    val list = Machine.list

    def anon(machine: Machine) = {
      val aliasId = Crypto.aliasify(DbAnonymize.machineAliasPrefixId, machine.machinePK.get)
      val aliasSerNum = Some(Crypto.aliasify(DbAnonymize.machineAliasPrefixSerialNumber, machine.machinePK.get))
      def id_real = Some(DbAnonymize.encrypt(machine.institutionPK, machine.id))
      def serNum_real = Some(DbAnonymize.encrypt(machine.institutionPK, machine.serialNumber.get))

      def anonId(m: Machine) = m.copy(id_real = id_real).copy(id = aliasId)

      def anonSerialNum(m: Machine) = m.copy(serialNumber = aliasSerNum).copy(serialNumber_real = serNum_real)

      val encryptId = machine.id_real.isEmpty

      val encryptSerNo = machine.serialNumber.isDefined && machine.serialNumber_real.isEmpty

      def update(m: Machine) = {
        def show(mch: Machine) = {
          "    id: " + mch.id.formatted("%-16s") +
            "    mch.id_real: " + mch.id_real.toString.formatted("%-70s") +
            "    mch.serialNumber: " + mch.serialNumber.toString.formatted("%-18s") +
            "    mch.serialNumber_real: " + mch.serialNumber_real.toString.formatted("%-70s")
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

    list.map(m => anon(m))
  }

  def transition = {
    anonymizeMachines
    logger.info("finished transition of anonymization security")
  }
}
