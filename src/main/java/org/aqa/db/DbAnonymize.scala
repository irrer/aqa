package org.aqa.db

//import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
import slick.jdbc.meta.MTable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import com.mchange.v2.c3p0.ComboPooledDataSource
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.Logging
import java.io.File
import org.aqa.Config
import scala.xml.XML
import slick.sql.FixedSqlAction
import org.aqa.Crypto
import edu.umro.ScalaUtil.Trace

//import slick.profile.FixedSqlAction   // TODO fix

/** Anonymize Database. */

object DbAnonymize extends Logging {

  def encode(institutionPK: Long, text: String): String = {
    ???
  }

  def machine = {
    val list = Machine.list

    def anon(machine: Machine) = {
      val aliasId = Crypto.aliasify(Machine.aliasPrefixId, machine.machinePK.get)
      val aliasSerNum = Some(Crypto.aliasify(Machine.aliasPrefixSerialNumber, machine.machinePK.get))
      def id_real = Some(encode(machine.institutionPK, machine.id))
      def serNum_real = Some(encode(machine.institutionPK, machine.serialNumber.get))

      def anonId(m: Machine) = m.copy(id_real = id_real).copy(id = aliasId)

      def anonSerialNum(m: Machine) = m.copy(serialNumber = aliasSerNum).copy(serialNumber_real = serNum_real)

      val encryptId = machine.id_real.isEmpty

      val encryptSerNo = machine.serialNumber.isDefined && machine.serialNumber_real.isEmpty

      def update(m: Machine) = {
        def show(mch: Machine) = "id: " + mch.id + "    mch.id_real: " + mch.id_real + "    mch.serialNumber: " + mch.serialNumber + "    mch.serialNumber_real: " + mch.serialNumber_real
        Trace.trace("Updating machine from : " + show(machine) + " ==> " + show(m))
        // TODO this is the toothless version, just showing what it would do but not actually changing anything.
        if (false) { // TODO make config flag for enabling
          // m.insertOrUpdate  // TODO make configuration flag for enabling
        }
      }

      (encryptId, encryptSerNo) match {
        case (true, true) => update(anonId(anonSerialNum(machine)))
        case (true, false) => update(anonId(machine))
        case (false, true) => update(anonSerialNum(machine))
        case (false, false) => ;
      }

    }

    list.map(m => anon(m))
  }

}
