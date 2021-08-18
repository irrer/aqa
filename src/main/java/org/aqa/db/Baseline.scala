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

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.db.Db.driver.api._

import java.sql.Timestamp

/**
  * Define values associated with specific machines that are established when the
  * machine is operating as expected.  These values can later be used to quantify
  * how much the values have changed.
  *
  * Note that <code>BaselineContent</code> records may be created to augment a
  * <code>Baseline</code> record.
  */
case class Baseline(
    baselinePK: Option[Long], // primary key
    maintenanceRecordPK: Long, // refers to maintenance for which to use this value
    acquisitionDate: Timestamp, // when data was acquired at the treatment machine.  Different from when this record was created.
    SOPInstanceUID: Option[String], // UID of DICOM image.  May be empty if not applicable.
    id: String, // unique identifier for data.  Can contain the concatenation of values such as beam name, energy level, jaw position, energy level, etc.  Should be human readable / user friendly
    value: String, // text version of value
    setup: String // <code>BaselineSetup</code> value
) {

  def insert: Baseline = {
    val insertQuery = Baseline.query returning Baseline.query.map(_.baselinePK) into ((baseline, baselinePK) => baseline.copy(baselinePK = Some(baselinePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(Baseline.query.insertOrUpdate(this))
}

object Baseline extends Logging {

  class BaselineTable(tag: Tag) extends Table[Baseline](tag, "baseline") {

    def baselinePK = column[Long]("baselinePK", O.PrimaryKey, O.AutoInc)

    def maintenanceRecordPK = column[Long]("maintenanceRecordPK")

    def acquisitionDate = column[Timestamp]("acquisitionDate")

    def SOPInstanceUID = column[Option[String]]("SOPInstanceUID")

    def id = column[String]("id")

    def value = column[String]("value")

    def setup = column[String]("setup")

    def * = (baselinePK.?, maintenanceRecordPK, acquisitionDate, SOPInstanceUID, id, value, setup) <> (Baseline.apply _ tupled, Baseline.unapply _)

    def maintenanceRecordFK =
      foreignKey("Baseline_maintenanceRecordPKConstraint", maintenanceRecordPK, MaintenanceRecord.query)(
        _.maintenanceRecordPK,
        onDelete = ForeignKeyAction.Cascade,
        onUpdate = ForeignKeyAction.Cascade
      )
  }

  val query = TableQuery[BaselineTable]

  def get(baselinePK: Long): Option[Baseline] = {
    val action = for {
      baseline <- query if baseline.baselinePK === baselinePK
    } yield baseline
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Given a machine and Baseline id, get the latest value on or before the given time stamp if it exists.
    */
  def findLatest(machinePK: Long, id: String, timestamp: Timestamp): Option[(MaintenanceRecord, Baseline)] = {
    // Special note: New baselines use the dataDate of the output.  Older baselines use the dataDate
    // of the individual slice.  The problem with the old way is that when searching for a baseline
    // with a timestamp, the only time available is the output time.  So a baseline will not be available
    // for the first data set.  Adding one hour should fix this (assuming that the treatment time was less
    // than one hour).
    val shiftedTimestamp = new Timestamp(timestamp.getTime + (60 * 60 * 1000))

    val action = {
      for {
        maintenanceRecord <- MaintenanceRecord.query.filter(m => m.machinePK === machinePK)
        baseline <- Baseline.query.filter(b => (b.id === id) && (b.maintenanceRecordPK === maintenanceRecord.maintenanceRecordPK) && (b.acquisitionDate <= shiftedTimestamp))
      } yield (maintenanceRecord, baseline)
    } sortBy (_._2.acquisitionDate.desc)

    val list = Db.run(action.result.headOption)
    if (list.isDefined)
      Some((list.get._1, list.get._2))
    else
      None
  }

  def delete(baselinePK: Long): Int = {
    val q = query.filter(_.baselinePK === baselinePK)
    logger.info("deleting baseline " + baselinePK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[Baseline]): Seq[Int] = {
    val ops = list.map { bl => Baseline.query.insertOrUpdate(bl) }
    Db.perform(ops)
  }

  /**
    * Given a set of maintenance records, only allow that are baselines and are referenced by baseline
    * values that have an ID that have text in the <code>requiredText</code> list.  Text
    * comparisons are case-insensitive.
    *
    * Note that this will filter out non-baseline maintenance events.
    */
  def filterOutUnrelatedBaselines(maintRecPKset: Set[Long], requiredText: Set[String]): Seq[MaintenanceRecord] = {
    val action = {
      for {
        maintenanceRecord <- MaintenanceRecord.query.filter(m => m.maintenanceRecordPK.inSet(maintRecPKset))
        baseline <- Baseline.query.filter(b => b.maintenanceRecordPK === maintenanceRecord.maintenanceRecordPK)
      } yield (maintenanceRecord, baseline.id)
    }

    val reqText = requiredText.map(t => t.toLowerCase)

    val list = Db.run(action.result)

    def idInSet(id: String) = {
      val idLo = id.toLowerCase
      val m = reqText.filter(rt => idLo.contains(rt))
      m.nonEmpty
    }

    val acceptable = list.filter(mb => !mb._1.category.equals(MaintenanceCategory.setBaseline) || idInSet(mb._2)).map(mb => mb._1)

    val result = acceptable.toList.groupBy(m => m.maintenanceRecordPK).values.map(v => v.head).toSeq.sortBy(_.creationTime.getTime)
    result
  }

  /**
    * Construct a baseline object using an attribute list.
    */
  def makeBaseline(maintenanceRecordPK: Long, dataDate: Timestamp, SOPInstanceUID: String, id: String, value: Double): Baseline = {
    new Baseline(None, maintenanceRecordPK, dataDate, Some(SOPInstanceUID), id, value.toString, BaselineSetup.byDefault.toString)
  }

  def main(args: Array[String]): Unit = {

    def setToBaseline(w: WedgePoint): Unit = {
      val isBsLn = w.copy(isBaseline = true)
      isBsLn.insertOrUpdate()
      println("Updated " + isBsLn)
    }

    DbSetup.init

    val action = WedgePoint.query.filter(_.isBaseline_text =!= "false")
    val list = Db.run(action.result)

    list.foreach(sf => setToBaseline(sf))

    Trace.trace("--- Finish baseline.  Changed text to boolean ----------------------------------------------------------")
    println("run with: java -cp  target\\AQA-0.3.1-jar-with-dependencies.jar org.aqa.db.Baseline")
  }
}
