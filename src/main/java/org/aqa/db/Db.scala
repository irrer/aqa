package org.aqa.db

import scala.concurrent.duration.DurationInt
import slick.jdbc.meta.MTable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import com.mchange.v2.c3p0.ComboPooledDataSource
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.Logging
import java.io.File
import org.aqa.Config
import scala.xml.XML
import slick.sql.FixedSqlAction
import edu.umro.ScalaUtil.Trace

/** Database utilities. */

object Db extends Logging {

  /** Ensure that the the configuration has been read. */
  private val configInit = Config.validate

  /**
   * Look at the DB configuration to determine which driver to load.
   */
  val driver = {
    val name = Config.SlickDb.getString("db.default.driver")
    logger.info("Using DB drive: " + name)
    val d = 0 match {
      case _ if name.toLowerCase.contains("postgres") => slick.driver.PostgresDriver
      case _ if name.toLowerCase.contains("sqlserver") => slick.jdbc.SQLServerProfile
      case _ if name.toLowerCase.contains("oracle") => slick.jdbc.OracleProfile
      case _ if name.toLowerCase.contains("mysql") => slick.driver.MySQLDriver
      case _ if name.toLowerCase.contains("h2") => slick.driver.H2Driver
      case _ if name.toLowerCase.contains("sqlite") => slick.driver.SQLiteDriver
      case _ if name.toLowerCase.contains("derby") => slick.driver.DerbyDriver
      case _ if name.toLowerCase.contains("hsqld") => slick.driver.HsqldbDriver
      case _ => throw new RuntimeException("Unable to recognize database driver: " + name)
    }
    logger.info("Using database driver " + d)
    d
  }

  import Db.driver.api._

  /**
   * Default timeout for general database overhead operations.  This includes operations like
   *  CREATE or DROP that should happen quickly, but does not include SELECT or other operations that could take longer.
   */
  val TIMEOUT = new DurationInt(60).seconds

  val db = {
    val prefix = "db.default.db"

    // log meaningful information regarding the database connection
    def fmt(key: String) = "    " + key + ": " + Config.SlickDb.getString(prefix + "." + key)
    val keyList = Seq("host", "port", "databaseName")
    val msg = "Database  " + keyList.map(k => fmt(k)).mkString("    ")
    logger.info("Attempting to connect to " + msg)

    // create the database from the config
    val d = try {
      val dd = Database.forConfig(prefix, Config.SlickDb)
      logger.info("Was able to connect to " + msg)
      dd
    } catch {
      case t: Throwable => {
        logger.error("Unable to create database connection: " + fmtEx(t))
        throw new RuntimeException("Unable to create database connection: " + fmtEx(t))
      }
    }
    d
  }

  private def tableName(table: TableQuery[Table[_]]): String = table.shaped.value.tableName

  def run[R](op: DBIOAction[R, NoStream, Nothing]): R = {
    val dbAction = db.run(op)
    val result = Await.result(dbAction, TIMEOUT)
    dbAction.onComplete {
      case Failure(ex) => throw (ex)
      case Success(data) => ;
    }
    result
  }

  def perform(dbOperation: driver.ProfileAction[Unit, NoStream, Effect.Schema]): Unit = {
    dbOperation.statements.foreach { s => logger.info("Executing database statement: " + s) }
    run(DBIO.seq(dbOperation))
  }

  // def perform(ops:  Seq[FixedSqlAction[Int, NoStream, Effect.Write]]) = {   // TODO fix
  def perform(ops: Seq[FixedSqlAction[Int, NoStream, Effect.Write]]) = {
    run(DBIO.sequence(ops))
  }

  /**
   * Determine if table exists in database
   */
  private def tableExists(table: TableQuery[Table[_]]): Boolean = {
    val tables = Await.result(db.run(MTable.getTables), TIMEOUT).toList
    val exists = tables.filter(m => m.name.name.equalsIgnoreCase(tableName(table))).size > 0
    exists
  }

  /**
   * If the given table exists, drop it.
   */
  def dropTableIfExists(table: TableQuery[Table[_]]): Unit = {
    if (tableExists(table)) {
      perform(table.schema.drop)
      if (tableExists(table)) throw new RuntimeException("Tried but failed to drop table " + tableName(table))
    }
  }

  def createTableIfNonexistent(table: TableQuery[Table[_]]): Unit = {
    if (!tableExists(table)) {
      perform(table.schema.create)
      if (!tableExists(table)) throw new RuntimeException("Tried but failed to create table " + tableName(table))
    }
  }

}