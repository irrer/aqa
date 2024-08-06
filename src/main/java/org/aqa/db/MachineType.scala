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

import org.aqa.db.Db.driver.api._
import org.aqa.Config
import org.aqa.webrun.phase2.phase2csv.MetadataCache

case class MachineType(
    machineTypePK: Option[Long], // primary key
    manufacturer: String, // company name
    model: String, // manufacturer's model name
    version: String, // details if manufacturer and model are not sufficiently unique
    notes: String // any extra information
) {

  def insert: MachineType = {
    MetadataCache.invalidate()
    val insertQuery = MachineType.query returning MachineType.query.map(_.machineTypePK) into ((machineType, machineTypePK) => machineType.copy(machineTypePK = Some(machineTypePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = {
    MetadataCache.invalidate()
    Db.run(MachineType.query.insertOrUpdate(this))
  }

  def toName: String = (model + " " + version).trim

  /** True if this is a Varian TrueBeam model. */
  //noinspection SpellCheckingInspection
  val isTrueBeam: Boolean = manufacturer.toLowerCase().contains("varian") && model.toLowerCase().contains("truebeam")

  /** True if this is a Varian C-Series model. */
  val isCSeries: Boolean = manufacturer.toLowerCase().contains("varian") && model.toLowerCase().contains("c-series")
}

object MachineType {
  class MachineTypeTable(tag: Tag) extends Table[MachineType](tag, "machineType") {

    def machineTypePK = column[Long]("machineTypePK", O.PrimaryKey, O.AutoInc)
    def manufacturer = column[String]("manufacturer")
    def model = column[String]("model")
    def version = column[String]("version")
    def notes = column[String]("notes")

    def * = (machineTypePK.?, manufacturer, model, version, notes) <> (MachineType.apply _ tupled, MachineType.unapply)
  }

  val query = TableQuery[MachineTypeTable]

  def get(machineTypePK: Long): Option[MachineType] = {
    val action = for {
      inst <- MachineType.query if inst.machineTypePK === machineTypePK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  def get(manufacturer: String, model: String, version: String): Option[MachineType] = {
    val action = for {
      inst <- MachineType.query if (inst.manufacturer.toLowerCase === manufacturer.toLowerCase) &&
        (inst.model.toLowerCase === model.toLowerCase) &&
        (inst.version.toLowerCase === version.toLowerCase)
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all machineTypes.
    */
  def list: Seq[MachineType] = Db.run(query.result)

  def delete(machineTypePK: Long): Int = {
    MetadataCache.invalidate()
    val q = query.filter(_.machineTypePK === machineTypePK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    Config.validate
    DbSetup.init
  }
}
