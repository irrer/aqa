package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import edu.umro.ScalaUtil.Trace

/**
 * Represent the Daily QA parameters for a single machine.  Note that this is not an
 * auto-incrementing table, and that the primary key is the same as the <code>Machine.machinePK</key>.
 *
 * This is only created if the user creates it, otherwise default values are used.
 */
case class MachineDailyQA(
  machineDailyQAPK: Option[Long], // primary key
  passLimit_mm: Double, // If composite results are under this, then they should be considered to have passed.
  warningLimit_mm: Double // If composite results are under this, then they should be considered to have passed, but are not as good as expected.
) {

  def insert: MachineDailyQA = {
    val insertQuery = MachineDailyQA.query returning MachineDailyQA.query.map(_.machineDailyQAPK) into ((machineDailyQA, machineDailyQAPK) => machineDailyQA.copy(machineDailyQAPK = Some(machineDailyQAPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(MachineDailyQA.query.insertOrUpdate(this))

  override def equals(o: Any): Boolean = {
    val other = o.asInstanceOf[MachineDailyQA]
    passLimit_mm.equals(other.passLimit_mm) &&
      warningLimit_mm.equals(other.warningLimit_mm)
  }

  override def toString = {
    "PK: " + machineDailyQAPK +
      "    passLimit_mm: " + Util.fmtDbl(passLimit_mm) +
      "    warningLimit_mm: " + Util.fmtDbl(warningLimit_mm)
  }
}

object MachineDailyQA {
  class MachineDailyQATable(tag: Tag) extends Table[MachineDailyQA](tag, "machineDailyQA") {

    def machineDailyQAPK = column[Long]("machineDailyQAPK", O.PrimaryKey)
    def passLimit_mm = column[Double]("passLimit_mm")
    def warningLimit_mm = column[Double]("warningLimit_mm")

    def * = (
      machineDailyQAPK.?,
      passLimit_mm,
      warningLimit_mm) <> ((MachineDailyQA.apply _)tupled, MachineDailyQA.unapply _)

    //def machineFK = foreignKey("MachineDailyQA_machinePKConstraint", machinePK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def machFK = foreignKey("MachineDailyQA_machinePKConstraint", machineDailyQAPK, Machine.query)(_.machinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[MachineDailyQATable]

  def get(machineDailyQAPK: Long): Option[MachineDailyQA] = {
    val action = for {
      inst <- MachineDailyQA.query if inst.machineDailyQAPK === machineDailyQAPK
    } yield (inst)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  /**
   * Get a list of all MachineDailyQA.
   */
  def list = Db.run(query.result)

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
      case _ => new MachineDailyQA(Some(machinePK), passLimit_mm = Config.DailyQAPassLimit_mm, warningLimit_mm = Config.DailyQAWarningLimit_mm)
    }
  }
}
