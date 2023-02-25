/*
 * Copyright 2023 Regents of the University of Michigan
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

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Db.driver.api._

/**
  * Represent the Winston Lutz parameters for a single machine.  Note that this is not an
  * auto-incrementing table, and that the primary key is the same as the <code>Machine.machinePK</key>.
  *
  * This is only created if the user creates it, otherwise default values are used.
  */
case class MachineWL(
    machineWLPK: Option[Long], // primary key
    passLimit_mm: Double, // If results are under this, then they should be considered to have passed.
    ballDiameter_mm: Double // Expected diameter of the ball for this machine.
) {

  def insert: MachineWL = {
    val insertQuery =
      MachineWL.query returning MachineWL.query.map(_.machineWLPK) into ((machineWL, machineWLPK) => machineWL.copy(machineWLPK = Some(machineWLPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(MachineWL.query.insertOrUpdate(this))

  override def equals(o: Any): Boolean = {
    val other = o.asInstanceOf[MachineWL]
    passLimit_mm.equals(other.passLimit_mm) &&
    ballDiameter_mm.equals(other.ballDiameter_mm)
  }

  override def toString: String = {
    "PK: " + machineWLPK +
      "    passLimit_mm: " + Util.fmtDbl(passLimit_mm) +
      "    ballDiameter_mm: " + Util.fmtDbl(ballDiameter_mm)
  }
}

object MachineWL {
  class MachineWLTable(tag: Tag) extends Table[MachineWL](tag, "machineWL") {

    def machineWLPK = column[Long]("machineWLPK", O.PrimaryKey)
    def passLimit_mm = column[Double]("passLimit_mm")
    def ballDiameter_mm = column[Double]("ballDiameter_mm")

    def * = (machineWLPK.?, passLimit_mm, ballDiameter_mm) <> (MachineWL.apply _ tupled, MachineWL.unapply)

    //def machineFK = foreignKey("MachineWL_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def machFK = foreignKey("MachineWL_machinePKConstraint", machineWLPK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineWLTable]

  def get(machineWLPK: Long): Option[MachineWL] = {
    val action = for {
      inst <- MachineWL.query if inst.machineWLPK === machineWLPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all MachineWL.
    */
  def list: Seq[MachineWL] = Db.run(query.result)

  case class MachineWLExtended(machine: Machine, machineType: MachineType, machineWL: MachineWL) {}

  /**
    * Get a list of Winston Lutz parameters with extended values to support showing the user a nice list.
    */
  def listExtended(instPK: Long): Seq[MachineWLExtended] = {
    val action = for {
      machine <- Machine.query if machine.institutionPK === instPK
      machineType <- MachineType.query if machineType.machineTypePK === machine.machineTypePK
    } yield (machine, machineType)

    //          machineWL <- query if machineWL.machineWLPK === machine.machinePK

    val machList = Db.run(action.result).map(mmm => (mmm._1, mmm._2))

    val result = machList.map(mmt => MachineWLExtended(mmt._1, mmt._2, getMachineWLOrDefault(mmt._1.machinePK.get)))
    result
  }

  def delete(machineWLPK: Long): Int = {
    val q = query.filter(_.machineWLPK === machineWLPK)
    val action = q.delete
    Db.run(action)
  }

  /**
    * Get the Winston Lutzlimits for the given machine.  If they do not exist, then get the default values.
    */
  def getMachineWLOrDefault(machinePK: Long): MachineWL = {
    MachineWL.get(machinePK) match {
      case Some(machineWL) => machineWL
      case _               => new MachineWL(Some(machinePK), passLimit_mm = Config.WLPassLimit, ballDiameter_mm = Config.WLBallDiameter)
    }
  }
}
