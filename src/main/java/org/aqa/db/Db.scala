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
import com.typesafe.config.ConfigFactory

/** Schema independent database utilities. */

object Db extends Logging {

  /** Ensure that the the configuration has been read. */
  private val configInit = Config.validate

  def isSqlServer = Config.SlickDb.getString("db.default.driver").toLowerCase.contains("sqlserver")

  /**
   * Look at the DB configuration to determine which driver to load.
   */
  val driver = {
    val name = Config.SlickDb.getString("db.default.driver")
    logger.info("Using DB drive: " + name)

    // This should never be true, but forces slick.jdbc.SQLServerProfile to be loaded and resolved.
    if (slick.jdbc.SQLServerProfile == null) logger.warn("slick.jdbc.SQLServerProfile is null")

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

  private def initDb = {
    val prefix = "db.default.db"
    Trace.trace("initializing db")
    // log meaningful information regarding the database connection
    def fmt(key: String) = "    " + key + ": " + Config.SlickDb.getString(prefix + "." + key)
    val keyList = Seq("host", "port", "databaseName")
    val msg = "Database  " + keyList.map(k => fmt(k)).mkString("    ")
    logger.info("Attempting to connect to " + msg)

    val j = Config.SlickDb.getString("db.default.driver") // TODO rm
    Trace.trace(j)

    // create the database from the config
    val d = try {
      // Using a configuration for Microsoft SQL Server does not work, but it does work using a Slick DataSource.
      val dd = if (isSqlServer) {
        logger.info("Configured for Microsoft SQL Server")
        val ds = new com.microsoft.sqlserver.jdbc.SQLServerDataSource
        def get(tag: String) = Config.SlickDb.getString("db.default.db." + tag)
        val userText = get("user").replace('/', '\\')
        Trace.trace(userText)
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
      case t: Throwable => {
        logger.error("Unable to create database connection: " + fmtEx(t))
        throw new RuntimeException("Unable to create database connection: " + fmtEx(t))
      }
    }
    d
  }

  private var dbValue: Database = null

  def db = TIMEOUT.synchronized {
    Trace.trace("db")
    if (dbValue == null) dbValue = initDb
    dbValue
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

  def perform(ops: Seq[FixedSqlAction[Int, NoStream, Effect.Write]]) = {
    Trace.trace(ops)
    run(DBIO.sequence(ops))
  }

  private object TableList {
    /** List of database tables in lower case. Do not reference this directly, instead use <code>getTableList</code> .*/
    private var tableList: Seq[String] = null

    def resetTableList = tableList = null

    /**
     * Get the latest version of the list of tables.
     */
    def getTableList = {
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
    Trace.trace
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