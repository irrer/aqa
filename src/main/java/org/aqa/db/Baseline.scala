package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import java.sql.Timestamp
import java.io.File
import org.aqa.web.WebServer
import edu.umro.ScalaUtil.FileUtil

/**
 * Define values associated with specific machines that are established when the
 * machine is operating as expected.  These values can later be used to quantify
 * how much the values have changed.
 */
case class Baseline(
  baselinePK: Option[Long], // primary key
  machinePK: Long, // machine for which to use this value
  acquisitionDate: Timestamp, // when data was acquired at the treatment machine
  installDate: Timestamp, // when the user decided to start using this value as the baseline
  userPK: Option[Long], // user who chose this baseline
  SOPInstanceUID: Option[String], // UID of DICOM image.  May be empty if not applicable.
  id: String, // unique identifier for data.  Can contain the concatenation of values such as beam name, energy level, jaw position, energy level, etc.  Should be human readable / user friendly
  value: String, // text version of value
  description: String // a description that helps the user understand what it is.  This is not used for computation.
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
    def machinePK = column[Long]("machinePK")
    def acquisitionDate = column[Timestamp]("acquisitionDate")
    def installDate = column[Timestamp]("installDate")
    def userPK = column[Option[Long]]("userPK")
    def SOPInstanceUID = column[Option[String]]("SOPInstanceUID")
    def id = column[String]("id")
    def value = column[String]("value")
    def description = column[String]("description")

    def * = (
      baselinePK.?,
      machinePK,
      acquisitionDate,
      installDate,
      userPK,
      SOPInstanceUID,
      id,
      value,
      description) <> ((Baseline.apply _)tupled, Baseline.unapply _)

    def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BaselineTable]

  def get(baselinePK: Long): Option[Baseline] = {
    val action = for {
      baseline <- query if baseline.baselinePK === baselinePK
    } yield (baseline)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(baselinePK: Long): Int = {
    val q = query.filter(_.baselinePK === baselinePK)
    logger.info("deleting baseline " + baselinePK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
  }
}
