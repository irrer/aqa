package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import java.sql.Timestamp
import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import edu.umro.ScalaUtil.Trace

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

  def insertOrUpdate = Db.run(Baseline.query.insertOrUpdate(this))
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

    def * = (
      baselinePK.?,
      maintenanceRecordPK,
      acquisitionDate,
      SOPInstanceUID,
      id,
      value,
      setup) <> ((Baseline.apply _)tupled, Baseline.unapply _)

    def maintenanceRecordFK = foreignKey("maintenanceRecordPK", maintenanceRecordPK, MaintenanceRecord.query)(_.maintenanceRecordPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BaselineTable]

  def get(baselinePK: Long): Option[Baseline] = {
    val action = for {
      baseline <- query if baseline.baselinePK === baselinePK
    } yield (baseline)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Given a machine and Baseline id, get the latest value on or before the given time stamp if it exists.
   */
  def findLatest(machinePK: Long, id: String, timestamp: Timestamp): Option[(MaintenanceRecord, Baseline)] = {
    val action = {
      for {
        maintenanceRecord <- MaintenanceRecord.query.filter(m => m.machinePK === machinePK)
        baseline <- Baseline.query.filter(b => (b.id === id) && (b.maintenanceRecordPK === maintenanceRecord.maintenanceRecordPK) && (b.acquisitionDate <= timestamp))
      } yield (maintenanceRecord, baseline)
    } sortBy (_._2.acquisitionDate.desc)

    Trace.trace(action.result.statements.mkString("\n"))

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

  def insert(list: Seq[Baseline]) = {
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
    Trace.trace
    val action = {
      for {
        maintenanceRecord <- MaintenanceRecord.query.filter(m => m.maintenanceRecordPK.inSet(maintRecPKset));
        baseline <- Baseline.query.filter(b => (b.maintenanceRecordPK === maintenanceRecord.maintenanceRecordPK))
      } yield (maintenanceRecord, baseline.id)
    }
    Trace.trace

    val reqText = requiredText.map(t => t.toLowerCase)
    Trace.trace

    val list = Db.run(action.result)

    Trace.trace(list.mkString("\n"))
    def idInSet(id: String) = {
      val idLo = id.toLowerCase
      val m = reqText.filter(rt => idLo.contains(rt))
      m.nonEmpty
    }
    Trace.trace
    val acceptable = list.filter(mb => ((!mb._1.category.equals(MaintenanceCategory.setBaseline))) || (idInSet(mb._2))).map(mb => mb._1)
    Trace.trace

    val result = acceptable.toList.groupBy(m => m.maintenanceRecordPK).values.map(v => v.head).toSeq.sortBy(_.creationTime.getTime)
    Trace.trace
    result
  }

  /**
   * Construct a baseline object using an attribute list.
   */
  def makeBaseline(maintenanceRecordPK: Long, attributeList: AttributeList, id: String, value: Double): Baseline = {
    val date = Util.extractDateTimeAndPatientIdFromDicomAl(attributeList)._1.head
    val SOPInstanceUID = Util.sopOfAl(attributeList)
    new Baseline(None, maintenanceRecordPK, new Timestamp(date.getTime), Some(SOPInstanceUID), id, value.toString, BaselineSetup.byDefault.toString)
  }
}
