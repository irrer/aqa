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
import org.aqa.Logging
import org.aqa.Util
import slick.jdbc.meta.MTable
import slick.sql.FixedSqlAction

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.util.Failure
import scala.util.Success

/** Schema independent database utilities. */

object Db extends Logging {

  /** Ensure that the the configuration has been read. */
  Config.validate

  def isSqlServer: Boolean = Config.SlickDb.getString("db.default.driver").toLowerCase.contains("sqlserver")

  /**
    * Look at the DB configuration to determine which driver to load.
    */
  val driver = {
    val name = Config.SlickDb.getString("db.default.driver")
    logger.info("Using DB drive: " + name)

    // This should never be true, but forces slick.jdbc.SQLServerProfile to be loaded and resolved.
    if (slick.jdbc.SQLServerProfile == null) logger.warn("slick.jdbc.SQLServerProfile is null")

    val d = 0 match {
      case _ if name.toLowerCase.contains("postgres")  => slick.jdbc.PostgresProfile
      case _ if name.toLowerCase.contains("sqlserver") => slick.jdbc.SQLServerProfile
      // case _ if name.toLowerCase.contains("oracle")    => slick.jdbc.OracleProfile // not tested
      // case _ if name.toLowerCase.contains("mysql")     => slick.jdbc.MySQLProfile // not tested
      // case _ if name.toLowerCase.contains("h2")        => slick.jdbc.H2Profile // not tested
      // case _ if name.toLowerCase.contains("sqlite")    => slick.jdbc.SQLiteProfile // not tested
      // case _ if name.toLowerCase.contains("derby")     => slick.jdbc.DerbyProfile // not tested
      // case _ if name.toLowerCase.contains("hsqld")     => slick.jdbc.HsqldbProfile // not tested
      case _ => throw new RuntimeException("Unable to recognize database driver: " + name)
    }
    logger.info("Using database driver " + d)
    d
  }

  import org.aqa.db.Db.driver.api._

  /**
    * Default timeout for general database overhead operations.  This includes operations like
    *  CREATE or DROP that should happen quickly, but does not include SELECT or other operations that could take longer.
    */
  val TIMEOUT: FiniteDuration = new DurationInt(60).seconds

  private def initDb = {
    val prefix = "db.default.db"
    // log meaningful information regarding the database connection
    def fmt(key: String) = "    " + key + ": " + Config.SlickDb.getString(prefix + "." + key)
    val keyList = Seq("host", "port", "databaseName")
    val msg = "Database  " + keyList.map(k => fmt(k)).mkString("    ")
    logger.info("Attempting to connect to " + msg)

    // create the database from the config
    val d =
      try {
        // Using a configuration for Microsoft SQL Server does not work, but it does work using a Slick DataSource.
        val dd = if (isSqlServer) {
          logger.info("Configured for Microsoft SQL Server")
          val ds = new com.microsoft.sqlserver.jdbc.SQLServerDataSource
          def get(tag: String) = Config.SlickDb.getString("db.default.db." + tag)
          val userText = get("user").replace('/', '\\')
          ds.setUser(userText)
          ds.setIntegratedSecurity(get("integratedSecurity").toBoolean)
          ds.setPassword(get("password"))
          ds.setPortNumber(get("port").toInt)
          ds.setURL(get("url"))
          ds.setDatabaseName(get("databaseName"))

          // prove that we can get a connection.  If this fails, then we can not connect to the database and it is better to know right away
          logger.info("Attempting to open connection to SQL Server database ...")
          val connection = ds.getConnection
          logger.info("Was able to open connection to SQL Server database: " + connection)

          val dbDs = Database.forDataSource(ds, None, AsyncExecutor.default())
          dbDs
        } else
          // for all the other databases that play nicely with config.
          Database.forConfig(prefix, Config.SlickDb)
        logger.info("Was able to configure " + msg)
        Some(dd)
      } catch {
        case t: Throwable =>
          logger.error("Unable to create database connection: " + fmtEx(t))
          throw new RuntimeException("Unable to create database connection: " + fmtEx(t))
      }
    d
  }

  private var dbValue: Option[Database] = None

  private def db: driver.api.Database =
    TIMEOUT.synchronized {
      if (dbValue.isEmpty) dbValue = initDb
      dbValue.get
    }

  private def tableName(table: TableQuery[Table[_]]): String = table.shaped.value.tableName

  private val dbSync = "Synchronize the database 4329832097432095743250743287325987687698769876"

  def run[R](op: DBIOAction[R, NoStream, Nothing]): R =
    dbSync.synchronized {

      /** A database operation that takes longer than this (in ms) is considered to have taken a very long time. */
      val veryLongTime_ms = 5 * 1000
      try {
        val start = System.currentTimeMillis()
        val dbAction = db.run(op)
        val result = Await.result(dbAction, TIMEOUT)
        val elapsed_ms = System.currentTimeMillis() - start
        if (elapsed_ms > veryLongTime_ms) {
          val stackText = Thread.currentThread.getStackTrace.tail
            .map(_.toString)
            .filterNot(_.contains("scala.collection")) // filter out stuff we don't care about
            .filterNot(_.contains("org.restlet")) // filter out stuff we don't care about
            .take(10)
            .map(se => "    " + se)
            .mkString("\n")
          logger.info("Database operation took the very long time of " + Util.elapsedTimeHumanFriendly(elapsed_ms) + " when called from\n" + stackText)
        }

        logger.info("Database elapsed time: " + Util.elapsedTimeHumanFriendly(elapsed_ms) + "    op: " + op.toString)

        dbAction.onComplete {
          case Failure(ex) =>
            val stackTrace = fmtEx(new RuntimeException("Db.run stack trace"))
            logger.warn("Error from database: " + fmtEx(ex) + "\nAQA source stack trace:" + stackTrace)
            throw ex
          case Success(_) =>
        }
        result
      } catch {
        case ex: Throwable =>
          val stackTrace = fmtEx(new RuntimeException("Db.run stack trace from Slick internal error"))
          val msg = "Error from Slick: " + fmtEx(ex) + "\nAQA source stack trace:" + stackTrace
          logger.warn(msg)
          throw new RuntimeException(msg)

      }

    }

  def perform(dbOperation: driver.ProfileAction[Unit, NoStream, Effect.Schema]): Unit = {
    dbOperation.statements.foreach { s => logger.info("Executing database statement: " + s) }
    run(DBIO.seq(dbOperation))
  }

  def perform(ops: Seq[FixedSqlAction[Int, NoStream, Effect.Write]]): Seq[Int] = {
    run(DBIO.sequence(ops))
  }

  private object TableList {

    /** List of database tables in lower case. Do not reference this directly, instead use <code>getTableList</code> .*/
    private var tableList: Seq[String] = Seq[String]()

    def resetTableList(): Unit = tableList = Seq[String]()

    /**
      * Get the latest version of the list of tables.
      */
    def getTableList: Seq[String] = {
      if (tableList.isEmpty) {
        val tableListMixed = if (isSqlServer) {
          val action = sql"select TABLE_NAME from INFORMATION_SCHEMA.TABLES".as[String]
          val tl = run(action)
          tl
        } else {
          Await.result(db.run(MTable.getTables), TIMEOUT).map(m => m.name.name)
        }
        tableList = tableListMixed.map(name => name.toLowerCase)
      }
      tableList
    }

    /**
      * Determine if table exists in database
      */
    def tableExists(table: TableQuery[Table[_]]): Boolean = {
      val exists = TableList.getTableList.contains(tableName(table).toLowerCase)
      exists
    }

  }

  /**
    * If the given table exists, drop it.
    */
  def dropTableIfExists(table: TableQuery[Table[_]]): Unit = {
    if (TableList.tableExists(table)) {
      perform(table.schema.drop)
      TableList.resetTableList()
      if (TableList.tableExists(table)) throw new RuntimeException("Tried but failed to drop table " + tableName(table))
    }
  }

  def createTableIfNonexistent(table: TableQuery[Table[_]]): Unit = {
    if (!TableList.tableExists(table)) {
      perform(table.schema.create)
      TableList.resetTableList()
      if (!TableList.tableExists(table)) throw new RuntimeException("Tried but failed to create table " + tableName(table))
    }
  }

}
