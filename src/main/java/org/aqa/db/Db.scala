package org.aqa.db

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver
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
      case _ if name.toLowerCase.contains("postgres")  => slick.driver.PostgresDriver
      case _ if name.toLowerCase.contains("sqlserver") => slick.jdbc.SQLServerProfile
      case _ if name.toLowerCase.contains("oracle")    => slick.jdbc.OracleProfile
      case _ if name.toLowerCase.contains("mysql")     => slick.driver.MySQLDriver
      case _ if name.toLowerCase.contains("h2")        => slick.driver.H2Driver
      case _ if name.toLowerCase.contains("sqlite")    => slick.driver.SQLiteDriver
      case _ if name.toLowerCase.contains("derby")     => slick.driver.DerbyDriver
      case _ if name.toLowerCase.contains("hsqld")     => slick.driver.HsqldbDriver
      case _                                           => throw new RuntimeException("Unable to recognize database driver: " + name)
    }
    logger.info("Using database driver " + d)
    d
  }

  import Db.driver.api._

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
        dd
      } catch {
        case t: Throwable =>
          logger.error("Unable to create database connection: " + fmtEx(t))
          throw new RuntimeException("Unable to create database connection: " + fmtEx(t))
      }
    d
  }

  private var dbValue: Database = null

  def db: driver.api.Database =
    TIMEOUT.synchronized {
      if (dbValue == null) dbValue = initDb
      dbValue
    }

  private def tableName(table: TableQuery[Table[_]]): String = table.shaped.value.tableName

  def run[R](op: DBIOAction[R, NoStream, Nothing]): R = {

    /** A database operation that takes longer than this (in ms) is considered to have taken a very long time. */
    val veryLongTime_ms = 5 * 1000
    try {
      val start = System.currentTimeMillis()
      val dbAction = db.run(op)
      val elapsed = System.currentTimeMillis() - start
      if (elapsed > veryLongTime_ms) {
        val stackText = Thread.currentThread
          .getStackTrace()
          .tail
          .map(_.toString)
          .filterNot(_.contains("scala.collection")) // filter out stuff we don't care about
          .filterNot(_.contains("org.restlet")) // filter out stuff we don't care about
          .take(10)
          .map(se => "    " + se)
          .mkString("\n")
        logger.info("Database operation took the very long time of " + Util.elapsedTimeHumanFriendly(elapsed) + " when called from\n" + stackText)
      }
      val result = Await.result(dbAction, TIMEOUT)
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
    private var tableList: Seq[String] = null

    def resetTableList(): Unit = tableList = null

    /**
      * Get the latest version of the list of tables.
      */
    def getTableList: Seq[String] = {
      if (tableList == null) {
        val tableListMixed = if (isSqlServer) {
          //val action = sql"select TABLE_NAME from AQAmsDV.INFORMATION_SCHEMA.TABLES".as[String]
          val action = sql"select TABLE_NAME from INFORMATION_SCHEMA.TABLES".as[String]
          val tl = run(action).toSeq
          tl
        } else {
          Await.result(db.run(MTable.getTables), TIMEOUT).toSeq.map(m => m.name.name)
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
      TableList.resetTableList
      if (TableList.tableExists(table)) throw new RuntimeException("Tried but failed to drop table " + tableName(table))
    }
  }

  def createTableIfNonexistent(table: TableQuery[Table[_]]): Unit = {
    if (!TableList.tableExists(table)) {
      perform(table.schema.create)
      TableList.resetTableList
      if (!TableList.tableExists(table)) throw new RuntimeException("Tried but failed to create table " + tableName(table))
    }
  }

}
