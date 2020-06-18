package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.sql.Timestamp
import java.util.Date

/**
 * Record of Preventive Maintenance Inspection.
 */

case class MaintenanceRecord(
  maintenanceRecordPK: Option[Long], // primary key
  category: String, // type of maintenance
  machinePK: Long, // machine that was maintained
  creationTime: Timestamp, // when this record was created
  userPK: Long, // user that performed or oversaw maintenance
  outputPK: Option[Long], // optional reference to a related Output
  summary: String, // short description of maintenance
  description: String // description of maintenance
) {

  /**
   * Return true of the user is from the same institution as the machine.
   * 
   * Note that this check was added because there was an instance on the AWS system of a user
   * creating a maintenance event in a machine that belonged to a different institution.
   */
  private def checkUser: Boolean = {
    val machInst = Machine.get(machinePK).get.institutionPK
    val userInst = User.get(userPK).get.institutionPK
    machInst == userInst
  }

  def insert: MaintenanceRecord = {
    if (!checkUser) throw new RuntimeException("User is not authorized to insert maintenance record because they are from a different institution than the machine.")
    val insertQuery = MaintenanceRecord.query returning MaintenanceRecord.query.map(_.maintenanceRecordPK) into ((maintenanceRecord, maintenanceRecordPK) => maintenanceRecord.copy(maintenanceRecordPK = Some(maintenanceRecordPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = {
    if (!checkUser) throw new RuntimeException("User is not authorized to insert or update maintenance record because they are from a different institution than the machine.")
    Db.run(MaintenanceRecord.query.insertOrUpdate(this))
  }
}

object MaintenanceRecord {
  class MaintenanceRecordTable(tag: Tag) extends Table[MaintenanceRecord](tag, "maintenanceRecord") {

    def maintenanceRecordPK = column[Long]("maintenanceRecordPK", O.PrimaryKey, O.AutoInc)
    def category = column[String]("category")
    def machinePK = column[Long]("machinePK")
    def creationTime = column[Timestamp]("creationTime")
    def userPK = column[Long]("userPK")
    def outputPK = column[Option[Long]]("outputPK")
    def summary = column[String]("summary")
    def description = column[String]("description")

    def * = (
      maintenanceRecordPK.?,
      category,
      machinePK,
      creationTime,
      userPK,
      outputPK,
      summary,
      description) <> ((MaintenanceRecord.apply _)tupled, MaintenanceRecord.unapply _)

    def machineFK = foreignKey("machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def userFK = foreignKey("userPKConstraint", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MaintenanceRecordTable]

  def get(maintenanceRecordPK: Long): Option[MaintenanceRecord] = {
    val action = for {
      inst <- MaintenanceRecord.query if inst.maintenanceRecordPK === maintenanceRecordPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByMachine(machinePK: Long): Seq[MaintenanceRecord] = {
    val action = for {
      maintenanceRecord <- MaintenanceRecord.query if (maintenanceRecord.machinePK === machinePK)
    } yield (maintenanceRecord)
    Db.run(action.result)
  }

  def getByUser(userPK: Long): Seq[MaintenanceRecord] = {
    val action = for {
      maintenanceRecord <- MaintenanceRecord.query if (maintenanceRecord.userPK === userPK)
    } yield (maintenanceRecord)
    Db.run(action.result)
  }

  /**
   * Get the list of MaintenanceRecord's that have a creation time between the given limits (inclusive), which
   * may be given in either order.  The list returned is ordered by creation time.
   */
  def getRange(machinePK: Long, lo: Date, hi: Date): Seq[MaintenanceRecord] = {
    val loTs = new Timestamp(Math.min(lo.getTime, hi.getTime))
    val hiTs = new Timestamp(Math.max(lo.getTime, hi.getTime))
    val action = for {
      inst <- MaintenanceRecord.query if (inst.machinePK === machinePK) && (inst.creationTime >= loTs) && (inst.creationTime <= hiTs)
    } yield (inst)
    val list = Db.run(action.result)
    list.sortWith((a, b) => a.creationTime.getTime < b.creationTime.getTime)
  }

  /**
   * Get a list of all MaintenanceRecord's.
   */
  def list = Db.run(query.result)

  def delete(maintenanceRecordPK: Long): Int = {
    val q = query.filter(_.maintenanceRecordPK === maintenanceRecordPK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
  }
}
