/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.db

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._

import java.sql.Timestamp
import java.text.SimpleDateFormat
import scala.xml.Elem
import scala.xml.Node
import scala.xml.Text
import scala.xml.XML

/**
  * Representation of a machine log (trajectory) entry.
  */
case class MachineLog(
    machineLogPK: Option[Long], // primary key
    machinePK: Long, // PK of machine this references
    outputPK: Long, // PK of output that created this row
    DateTimeSaved: Timestamp, // date+time from log entry
    LoggedInUser: String, // LoggedInUser from log entry.  Probably the Varian service user ID.  NOT an AQA user.
    SystemVersion: String, // SystemVersion from log entry
    ServiceSoftwareVersion: String, // ServiceSoftwareVersion from log entry
    RTSSVersion: String, // RTSSVersion from log entry
    content: String // XML content
) {

  def insert: MachineLog = {
    val insertQuery = MachineLog.query returning MachineLog.query.map(_.machineLogPK) into
      ((machineLog, machineLogPK) => machineLog.copy(machineLogPK = Some(machineLogPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(MachineLog.query.insertOrUpdate(this))

  def elem: Elem = XML.loadString(content)

  override def toString: String = {
    val text = DateTimeSaved + " " + "    machinePK: " + machinePK
    text
  }
}

object MachineLog extends Logging {
  class MachineLogTable(tag: Tag) extends Table[MachineLog](tag, "machineLog") {

    def machineLogPK = column[Long]("machineLogPK", O.PrimaryKey, O.AutoInc)
    def machinePK = column[Long]("machinePK")
    def outputPK = column[Long]("outputPK")
    def DateTimeSaved = column[Timestamp]("DateTimeSaved")
    private def LoggedInUser = column[String]("LoggedInUser")
    def SystemVersion = column[String]("SystemVersion")
    private def ServiceSoftwareVersion = column[String]("ServiceSoftwareVersion")
    private def RTSSVersion = column[String]("RTSSVersion")
    def content = column[String]("content")

    def * =
      (
        machineLogPK.?,
        machinePK,
        outputPK,
        DateTimeSaved,
        LoggedInUser,
        SystemVersion,
        ServiceSoftwareVersion,
        RTSSVersion,
        content
      ) <> (MachineLog.apply _ tupled, MachineLog.unapply)

    def machineFK =
      foreignKey("machineLog_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)

    def outputFK =
      foreignKey("machineLog_outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineLogTable]

  private val dateFormat = new SimpleDateFormat("EEEE, dd MMMM yyyy HH:mm:ss")

  /**
    * Get the timestamp from the XML.
    * @param xml Representation of a Machine Log entry.
    * @return DateTimeSaved, or nothing on failure.
    */
  def getDateTimeSaved(xml: Elem): Option[Timestamp] = {
    try {
      val text = (xml \ "DateTimeSaved").head.text
      val date = dateFormat.parse(text)
      Some(new Timestamp(date.getTime))
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Construct a MachineLog instance from XML.
    * @param elem Machine log XML.
    * @return A MachineLog instance or nothing.
    */
  def construct(elem: Elem, outputPK: Long): Option[MachineLog] = {
    try {
      def env(tag: String) = {
        (elem \ "Environment" \ tag).head.text
      }

      val loggedInUser: String = {
        val text = (elem \ "LoggedInUser").head.text
        text
      }

      val machinePK: Long = {
        val deviceSerialNumber = Util.machineLogSerialNumber(elem)
        val machineList = Machine.findMachinesBySerialNumber(deviceSerialNumber.get)
        if (machineList.isEmpty)
          logger.warn("Could not find machine with device serial number " + deviceSerialNumber)
        machineList.head.machinePK.get
      }

      val machineLog = MachineLog(
        machineLogPK = None,
        machinePK,
        outputPK = outputPK,
        DateTimeSaved = getDateTimeSaved(elem).get,
        LoggedInUser = loggedInUser,
        SystemVersion = env("SystemVersion"),
        ServiceSoftwareVersion = env("ServiceSoftwareVersion"),
        RTSSVersion = env("RTSSVersion"),
        content = Util.prettyPrint(elem)
      )

      Some(machineLog)
    } catch {
      case t: Throwable =>
        logger.error("Unexpected exception parsing MachineLog XML: " + fmtEx(t))
        None
    }
  }

  def construct(xmlText: String, outputPK: Long): Option[MachineLog] = construct(XML.loadString(xmlText), outputPK)

  /**
    * Get a specific row.
    *
    * @param machineLogPK PK of entry.
    * @return Value, if it exists.
    */
  def get(machineLogPK: Long): Option[MachineLog] = {
    val action = for {
      inst <- MachineLog.query if inst.machineLogPK === machineLogPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  def get(machinePK: Long, date: Timestamp): Option[MachineLog] = {
    val action = for {
      inst <- MachineLog.query if (inst.machinePK === machinePK) && (inst.DateTimeSaved === date)
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  def get(machinePK: Long, dateSet: Set[Timestamp]): Seq[MachineLog] = {
    val action = for {
      inst <- MachineLog.query if (inst.machinePK === machinePK) && inst.DateTimeSaved.inSet(dateSet)
    } yield inst
    val list = Db.run(action.result)
    list
  }

  /**
    * Get the sorted list of dates from all machine log entries for the given machine.
    * @param machinePK For this machine.
    * @return
    */
  def getDateList(machinePK: Long): Seq[Timestamp] = {
    val action = MachineLog.query.filter(mach => mach.machinePK === machinePK).map(_.DateTimeSaved)
    val list = Db.run(action.result)
    list.sortBy(_.getTime)
  }

  def delete(machineLogPK: Long): Int = {
    val q = query.filter(_.machineLogPK === machineLogPK)
    val action = q.delete
    Db.run(action)
  }

  /**
    * Anonymize the machine serial number (aka: DeviceSerialNumber) in the given XML.
    *
    * @param elem Machine log as XML document.
    * @param newDeviceSerialNumber New value for serial number.
    *
    * @return Same XML with device serial number anonymized.
    */
  def anonymizeSerialNumber(elem: Elem, newDeviceSerialNumber: String): Node = {
    import scala.xml.Node
    import scala.xml.transform.RewriteRule
    import scala.xml.transform.RuleTransformer

    object t1 extends RewriteRule {
      override def transform(n: Node): Seq[Node] =
        n match {
          case Elem(prefix, "MachineSerialNumber", attributes, scope, _*) =>
            Elem(prefix, "MachineSerialNumber", attributes, scope, false, Text(newDeviceSerialNumber))
          case other => other
        }
    }

    object rt1 extends RuleTransformer(t1)

    object t2 extends RewriteRule {
      override def transform(n: Node): Seq[Node] =
        n match {
          case sn @ Elem(_, "Environment", _, _, _*) => rt1(sn)
          case other                                 => other
        }
    }

    object rt2 extends RuleTransformer(t2)

    val anonymized = rt2(elem)
    anonymized
  }

}
