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

import java.io.File
import java.sql.Timestamp
import java.text.SimpleDateFormat
import scala.xml.Elem
import scala.xml.Node
import scala.xml.PrettyPrinter
import scala.xml.Text
import scala.xml.XML

/**
  * Representation of a machine log (trajectory) entry.
  */
case class MachineLog(
    machineLogPK: Option[Long], // primary key
    machinePK: Long, // PK of machine this references
    DateTimeSaved: Timestamp, // date+time from log entry
    LoggedInUser: String, // LoggedInUser from log entry.  Probably the Varian service user ID.  NOT an AQA user.
    SystemVersion: String, // SystemVersion from log entry
    ServiceSoftwareVersion: String, // ServiceSoftwareVersion from log entry
    RTSSVersion: String, // RTSSVersion from log entry
    isBeamGenerationModule: Boolean, // True if log entry contains Node with name Beam Generation Module
    isCollimator: Boolean, // True if log entry contains Node with name Collimator
    isCouch: Boolean, // True if log entry contains Node with name Couch
    isKiloVoltageDetector: Boolean, // True if log entry contains Node with name Kilo Voltage Detector
    isKiloVoltageSource: Boolean, // True if log entry contains Node with name Kilo Voltage Source
    isMegaVoltageDetector: Boolean, // True if log entry contains Node with name Mega Voltage Detector
    isStand: Boolean, // True if log entry contains Node with name Stand
    isSupervisor: Boolean, // True if log entry contains Node with name Supervisor
    isXRayImager: Boolean, // True if log entry contains Node with name XRay Imager
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
    val node =
      Seq(
        if (isBeamGenerationModule) "isBeamGenerationModule" else "",
        if (isCollimator) "isCollimator" else "",
        if (isCouch) "isCouch" else "",
        if (isKiloVoltageDetector) "isKiloVoltageDetector" else "",
        if (isKiloVoltageSource) "isKiloVoltageSource" else "",
        if (isMegaVoltageDetector) "isMegaVoltageDetector" else "",
        if (isStand) "isStand" else "",
        if (isSupervisor) "isSupervisor" else "",
        if (isXRayImager) "isXRayImager" else ""
      ).filter(_.nonEmpty).mkString("  |  ")
    val text = DateTimeSaved + " " + "    machinePK: " + machinePK + "   Node(s): " + node
    text
  }
}

object MachineLog extends Logging {
  class MachineLogTable(tag: Tag) extends Table[MachineLog](tag, "machineLog") {

    def machineLogPK = column[Long]("machineLogPK", O.PrimaryKey, O.AutoInc)
    def machinePK = column[Long]("machinePK")
    def DateTimeSaved = column[Timestamp]("DateTimeSaved")
    def LoggedInUser = column[String]("LoggedInUser")
    def SystemVersion = column[String]("SystemVersion")
    def ServiceSoftwareVersion = column[String]("ServiceSoftwareVersion")
    def RTSSVersion = column[String]("RTSSVersion")
    def isBeamGenerationModule = column[Boolean]("isBeamGenerationModule")
    def isCollimator = column[Boolean]("isCollimator")
    def isCouch = column[Boolean]("isCouch")
    def isKiloVoltageDetector = column[Boolean]("isKiloVoltageDetector")
    def isKiloVoltageSource = column[Boolean]("isKiloVoltageSource")
    def isMegaVoltageDetector = column[Boolean]("isMegaVoltageDetector")
    def isStand = column[Boolean]("isStand")
    def isSupervisor = column[Boolean]("isSupervisor")
    def isXRayImager = column[Boolean]("isXRayImager")
    def content = column[String]("content")

    def * =
      (
        machineLogPK.?,
        machinePK,
        DateTimeSaved,
        LoggedInUser,
        SystemVersion,
        ServiceSoftwareVersion,
        RTSSVersion,
        isBeamGenerationModule,
        isCollimator,
        isCouch,
        isKiloVoltageDetector,
        isKiloVoltageSource,
        isMegaVoltageDetector,
        isStand,
        isSupervisor,
        isXRayImager,
        content
      ) <> (MachineLog.apply _ tupled, MachineLog.unapply)

    def machineFK =
      foreignKey("machineLog_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineLogTable]

  private val prettyPrinter = new PrettyPrinter(1024, 2)

  private val dateFormat = new SimpleDateFormat("EEEE, dd MMMM yyyy HH:mm:ss")

  /**
    * Construct a MachineLog instance from XML.
    * @param elem Machine log XML.
    * @return A MachineLog instance or nothing.
    */
  def construct(elem: Elem): Option[MachineLog] = {
    try {
      def env(tag: String) = {
        (elem \ "Environment" \ tag).head.text
      }

      def hasNode(tag: String): Boolean = {
        val parentList = elem \ "Node"
        val list = parentList.map(n => (n \ "@name").head.text)
        val has = list.contains(tag)
        has
      }

      val dateTimeSaved: Timestamp = {
        val text = (elem \ "DateTimeSaved").head.text
        val date = dateFormat.parse(text)
        new Timestamp(date.getTime)
      }

      val loggedInUser: String = {
        val text = (elem \ "LoggedInUser").head.text
        text
      }

      val machinePK: Long = {
        val deviceSerialNumber = env(tag = "MachineSerialNumber")
        val machineList = Machine.findMachinesBySerialNumber(deviceSerialNumber)
        if (machineList.isEmpty)
          logger.warn("Could not find machine with device serial number " + deviceSerialNumber)
        machineList.head.machinePK.get
      }

      val machineLog = MachineLog(
        machineLogPK = None,
        machinePK,
        DateTimeSaved = dateTimeSaved,
        LoggedInUser = loggedInUser,
        SystemVersion = env("SystemVersion"),
        ServiceSoftwareVersion = env("ServiceSoftwareVersion"),
        RTSSVersion = env("RTSSVersion"),
        isBeamGenerationModule = hasNode("Beam Generation Module"),
        isCollimator = hasNode("Collimator"),
        isCouch = hasNode("Couch"),
        isKiloVoltageDetector = hasNode("Kilo Voltage Detector"),
        isKiloVoltageSource = hasNode("Kilo Voltage Source"),
        isMegaVoltageDetector = hasNode("Mega Voltage Detector"),
        isStand = hasNode("Stand"),
        isSupervisor = hasNode("Supervisor"),
        isXRayImager = hasNode("XRay Imager"),
        content = prettyPrinter.format(elem)
      )

      Some(machineLog)
    } catch {
      case t: Throwable =>
        logger.error("Unexpected exception parsing MachineLog XML: " + fmtEx(t))
        None
    }
  }

  def construct(xmlText: String): Option[MachineLog] = construct(XML.loadString(xmlText))

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

  def main(args: Array[String]): Unit = {
    DbSetup.init
    println("Starting ...")
    //val dir = new File("""D:\tmp\aqa\MachineLogs\CedarsSinia""")
    val dir = new File("""D:\tmp\aqa\MachineLogs\H192448\H192448""")

    def showFile(f: File): Unit = {

      val text = Util.readTextFile(f).right.get
      val ml = MachineLog.construct(text)
      println(f.getName + "\n    " + ml.get)
      anonymizeSerialNumber(XML.loadString(text), "somethingBetter")
    }

    Util.listDirFiles(dir).filter(_.getName.endsWith(".xml")).foreach(showFile)
  }
}
