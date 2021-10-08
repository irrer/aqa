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
import org.aqa.db.Db.driver.api._

import java.sql.Timestamp
import scala.xml.Elem
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
    NodeBeamGenerationModule: Boolean, // True if log entry contains Node with name Beam Generation Module
    NodeCollimator: Boolean, // True if log entry contains Node with name Collimator
    NodeCouch: Boolean, // True if log entry contains Node with name Couch
    NodeKiloVoltageDetector: Boolean, // True if log entry contains Node with name Kilo Voltage Detector
    NodeKiloVoltageSource: Boolean, // True if log entry contains Node with name Kilo Voltage Source
    NodeMegaVoltageDetector: Boolean, // True if log entry contains Node with name Mega Voltage Detector
    NodeStand: Boolean, // True if log entry contains Node with name Stand
    NodeSupervisor: Boolean, // True if log entry contains Node with name Supervisor
    NodeXRayImager: Boolean, // True if log entry contains Node with name XRay Imager
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
        if (NodeBeamGenerationModule) "NodeBeamGenerationModule" else "",
        if (NodeCollimator) "NodeCollimator" else "",
        if (NodeCouch) "NodeCouch" else "",
        if (NodeKiloVoltageDetector) "NodeKiloVoltageDetector" else "",
        if (NodeKiloVoltageSource) "NodeKiloVoltageSource" else "",
        if (NodeMegaVoltageDetector) "NodeMegaVoltageDetector" else "",
        if (NodeStand) "NodeStand" else "",
        if (NodeSupervisor) "NodeSupervisor" else "",
        if (NodeXRayImager) "NodeXRayImager" else ""
      ).filter(_.nonEmpty).mkString("  |  ")
    val text = DateTimeSaved + " " + "    machinePK: " + machinePK + "   Node: " + node
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
    def NodeBeamGenerationModule = column[Boolean]("NodeBeamGenerationModule")
    def NodeCollimator = column[Boolean]("NodeCollimator")
    def NodeCouch = column[Boolean]("NodeCouch")
    def NodeKiloVoltageDetector = column[Boolean]("NodeKiloVoltageDetector")
    def NodeKiloVoltageSource = column[Boolean]("NodeKiloVoltageSource")
    def NodeMegaVoltageDetector = column[Boolean]("NodeMegaVoltageDetector")
    def NodeStand = column[Boolean]("NodeStand")
    def NodeSupervisor = column[Boolean]("NodeSupervisor")
    def NodeXRayImager = column[Boolean]("NodeXRayImager")
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
        NodeBeamGenerationModule,
        NodeCollimator,
        NodeCouch,
        NodeKiloVoltageDetector,
        NodeKiloVoltageSource,
        NodeMegaVoltageDetector,
        NodeStand,
        NodeSupervisor,
        NodeXRayImager,
        content
      ) <> (MachineLog.apply _ tupled, MachineLog.unapply)

    def machineFK =
      foreignKey("machineLog_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineLogTable]

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
}
