package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.sql.Timestamp

/**
 * Record of Preventive Maintenance Inspection.
 */

case class PMI(
  pmiPK: Option[Long], // primary key
  machinePK: Long, // machine that was maintained
  creationTime: Timestamp, // when this record was created
  userPK: Long, // user that performed or oversaw maintenance
  summary: String, // short description of maintenance
  description: String // description of maintenance
) {

  def insert: PMI = {
    val insertQuery = PMI.query returning PMI.query.map(_.pmiPK) into ((pmi, pmiPK) => pmi.copy(pmiPK = Some(pmiPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(PMI.query.insertOrUpdate(this))
}

object PMI {
  class PMITable(tag: Tag) extends Table[PMI](tag, "pmi") {

    def pmiPK = column[Long]("pmiPK", O.PrimaryKey, O.AutoInc)
    def machinePK = column[Long]("machinePK")
    def creationTime = column[Timestamp]("creationTime")
    def userPK = column[Long]("userPK")
    def summary = column[String]("summary")
    def description = column[String]("description")

    def * = (
      pmiPK.?,
      machinePK,
      creationTime,
      userPK,
      summary,
      description) <> ((PMI.apply _)tupled, PMI.unapply _)

    def machineFK = foreignKey("machinePK", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def userFK = foreignKey("userPK", userPK, User.query)(_.userPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PMITable]

  def get(pmiPK: Long): Option[PMI] = {
    val action = for {
      inst <- PMI.query if inst.pmiPK === pmiPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByMachine(machinePK: Long): Seq[PMI] = {
    val action = for {
      inst <- PMI.query if (inst.machinePK === machinePK)
    } yield (inst)
    Db.run(action.result)
  }

  /**
   * Get a list of all PMI's.
   */
  def list = Db.run(query.result)

  def delete(pmiPK: Long): Int = {
    val q = query.filter(_.pmiPK === pmiPK)
    val action = q.delete
    Db.run(action)
  }

  def main(args: Array[String]): Unit = {
    val valid = Config.validate
    DbSetup.init
  }
}
