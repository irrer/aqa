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

case class MultileafCollimator(
    multileafCollimatorPK: Option[Long], // primary key
    manufacturer: String, // manufacturer's name
    model: String, // manufacturer's model name
    version: String, // details if manufacturer and model are not sufficiently unique
    outerLeafPairCount: Int, // total number of opposing outer leaf pairs
    innerLeafPairCount: Int, // total number of opposing inner leaf pairs
    outerLeafWidth_cm: Double, // width of each outer leaf in cm
    innerLeafWidth_cm: Double, // width of each inner leaf in cm
    leafTravelDistance_cm: Double, // distance that a leaf can move in cm
    notes: String // any extra information
) {

  def insert: MultileafCollimator = {
    val insertQuery = MultileafCollimator.query returning MultileafCollimator.query.map(_.multileafCollimatorPK) into ((multileafCollimator, multileafCollimatorPK) =>
      multileafCollimator.copy(multileafCollimatorPK = Some(multileafCollimatorPK))
    )
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(MultileafCollimator.query.insertOrUpdate(this))

  def toName: String = (manufacturer + " " + model + " " + version).trim
}

object MultileafCollimator {
  class MultileafCollimatorTable(tag: Tag) extends Table[MultileafCollimator](tag, "multileafCollimator") {

    def multileafCollimatorPK = column[Long]("multileafCollimatorPK", O.PrimaryKey, O.AutoInc)
    def manufacturer = column[String]("manufacturer")
    def model = column[String]("model")
    def version = column[String]("version")
    def outerLeafPairCount = column[Int]("outerLeafPairCount")
    def innerLeafPairCount = column[Int]("innerLeafPairCount")
    def outerLeafWidth_cm = column[Double]("outerLeafWidth_cm")
    def innerLeafWidth_cm = column[Double]("innerLeafWidth_cm")
    def leafTravelDistance_cm = column[Double]("leafTravelDistance_cm")
    def notes = column[String]("notes")

    def * =
      (
        multileafCollimatorPK.?,
        manufacturer,
        model,
        version,
        outerLeafPairCount,
        innerLeafPairCount,
        outerLeafWidth_cm,
        innerLeafWidth_cm,
        leafTravelDistance_cm,
        notes
      ) <> ((MultileafCollimator.apply _) tupled, MultileafCollimator.unapply _)
  }

  val query = TableQuery[MultileafCollimatorTable]

  def get(multileafCollimatorPK: Long): Option[MultileafCollimator] = {
    val action = for {
      inst <- MultileafCollimator.query if inst.multileafCollimatorPK === multileafCollimatorPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def get(manufacturer: String, model: String, version: String): Option[MultileafCollimator] = {
    val action = for {
      inst <- MultileafCollimator.query if (inst.manufacturer.toLowerCase === manufacturer.toLowerCase) &&
        (inst.model.toLowerCase === model.toLowerCase) &&
        (inst.version.toLowerCase === version.toLowerCase)
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
    * Get a list of all multileafCollimators.
    */
  def list = Db.run(query.result)

  def delete(multileafCollimatorPK: Long): Int = {
    val q = query.filter(_.multileafCollimatorPK === multileafCollimatorPK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
    println("======== inst: " + get(5))
    println("======== inst delete: " + delete(5))
  }
}
