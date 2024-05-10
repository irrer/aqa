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

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Db.driver.api._

/**
  * Represent the Daily QA parameters for a single machine.  Note that this is not an
  * auto-incrementing table, and that the primary key is the same as the <code>Machine.machinePK</key>.
  *
  * This is only created if the user creates it, otherwise default values are used.
  */
case class MachineDailyQA(
    machineDailyQAPK: Option[Long], // primary key
    passLimit_mm: Double, // If composite results are under this, then they should be considered to have passed.
    warningLimit_mm: Double, // If composite results are under this, then they should be considered to have passed, but are not as good as expected.
    requireXRayOffset: Boolean
) {

  def insert: MachineDailyQA = {
    val insertQuery =
      MachineDailyQA.query returning MachineDailyQA.query.map(_.machineDailyQAPK) into ((machineDailyQA, machineDailyQAPK) => machineDailyQA.copy(machineDailyQAPK = Some(machineDailyQAPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(MachineDailyQA.query.insertOrUpdate(this))

  override def equals(o: Any): Boolean = {
    val other = o.asInstanceOf[MachineDailyQA]
    passLimit_mm.equals(other.passLimit_mm) &&
    warningLimit_mm.equals(other.warningLimit_mm) &&
    requireXRayOffset.toString.equals(other.warningLimit_mm.toString)
  }

  override def toString: String = {
    "PK: " + machineDailyQAPK +
      "    passLimit_mm: " + Util.fmtDbl(passLimit_mm) +
      "    warningLimit_mm: " + Util.fmtDbl(warningLimit_mm) +
      "    requireXRayOffset: " + requireXRayOffset.toString
  }
}

object MachineDailyQA {
  class MachineDailyQATable(tag: Tag) extends Table[MachineDailyQA](tag, "machineDailyQA") {

    def machineDailyQAPK = column[Long]("machineDailyQAPK", O.PrimaryKey)
    def passLimit_mm = column[Double]("passLimit_mm")
    def warningLimit_mm = column[Double]("warningLimit_mm")
    def requireXRayOffset = column[Boolean]("requireXRayOffset")

    def * = (machineDailyQAPK.?, passLimit_mm, warningLimit_mm, requireXRayOffset) <> (MachineDailyQA.apply _ tupled, MachineDailyQA.unapply)

    //def machineFK = foreignKey("MachineDailyQA_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def machFK = foreignKey("MachineDailyQA_machinePKConstraint", machineDailyQAPK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineDailyQATable]

  def get(machineDailyQAPK: Long): Option[MachineDailyQA] = {
    val action = for {
      inst <- MachineDailyQA.query if inst.machineDailyQAPK === machineDailyQAPK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all MachineDailyQA.
    */
  def list: Seq[MachineDailyQA] = Db.run(query.result)

  case class MachineDailyQAExtended(machine: Machine, machineType: MachineType, machineDailyQA: MachineDailyQA) {}

  /**
    * Get a list of Daily QA parameters with extended values to support showing the user a nice list.
    */
  def listExtended(instPK: Long): Seq[MachineDailyQAExtended] = {
    val action = for {
      machine <- Machine.query if machine.institutionPK === instPK
      machineType <- MachineType.query if machineType.machineTypePK === machine.machineTypePK
    } yield (machine, machineType)

    //          machineDailyQA <- query if machineDailyQA.machineDailyQAPK === machine.machinePK

    val machList = Db.run(action.result).map(mmm => (mmm._1, mmm._2))

    val result = machList.map(mmt => MachineDailyQAExtended(mmt._1, mmt._2, getMachineDailyQAOrDefault(mmt._1.machinePK.get)))
    result
  }

  def delete(machineDailyQAPK: Long): Int = {
    val q = query.filter(_.machineDailyQAPK === machineDailyQAPK)
    val action = q.delete
    Db.run(action)
  }

  /**
    * Get the Daily QA limits for the given machine.  If they do not exist, then get the default values.
    */
  def getMachineDailyQAOrDefault(machinePK: Long): MachineDailyQA = {
    MachineDailyQA.get(machinePK) match {
      case Some(machineDailyQA) => machineDailyQA
      case _                    => new MachineDailyQA(Some(machinePK), passLimit_mm = Config.DailyQAPassLimit_mm, warningLimit_mm = Config.DailyQAWarningLimit_mm, requireXRayOffset = false)
    }
  }
}
