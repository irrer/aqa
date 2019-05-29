package org.aqa.db

//import slick.backend.DatabaseConfig
import slick.driver.PostgresDriver
import scala.concurrent.duration.DurationInt
import slick.driver.PostgresDriver.api._
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
//import slick.profile.FixedSqlAction   // TODO fix

/** Database utilities. */

object Db extends Logging {

  /** Ensure that the the configuration has been read. */
  private val configInit = Config.validate

  /**
   * Default timeout for general database overhead operations.  This includes operations like
   *  CREATE or DROP that should happen quickly, but does not include SELECT or other operations that could take longer.
   */
  val TIMEOUT = new DurationInt(60).seconds

  //private val dbConfig: DatabaseConfig[PostgresDriver] = DatabaseConfig.forConfig("slick.dbs.default")

  val db = {
    val ds = new ComboPooledDataSource
    ds.setDriverClass(System.getProperty("slick.dbs.default.db.driver")) //  ds.setDriverClass(Driver)
    ds.setJdbcUrl(System.getProperty("slick.dbs.default.db.url")) //   ds.setJdbcUrl(Local)
    ds.setUser(System.getProperty("slick.dbs.default.db.user"))
    ds.setPassword(System.getProperty("slick.dbs.default.db.password"))
    Database.forDataSource(ds, Some(10)) // TODO fix
    // Database.forDataSource(ds)  // TODO fix
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

  def perform(dbOperation: PostgresDriver.DriverAction[Unit, NoStream, Effect.Schema]): Unit = {
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

  // ==========================================================================

  def main(args: Array[String]): Unit = {
    //val valid = Config.validate

    val dba = {
      val ds = new ComboPooledDataSource
      ds.setDriverClass("slick.driver.PostgresDriver$")
      ds.setJdbcUrl("jdbc:postgresql://aqa.cek8wjwn06iu.us-west-2.rds.amazonaws.com:5432/AQA?sslmode=require")
      ds.setUser("aqa")
      ds.setPassword("Kwalitee_1")
      Database.forDataSource(???, ???) // TODO fix
      //Database.forDataSource(ds,  maxConnections = 5, executor, keepAliveConnection) // TODO fix
      val maxConnections = Some(5)
      val executor = 1
      val keepAliveConnection = 1
      Database.forDataSource(ds, maxConnections) // , executor, keepAliveConnection) // TODO fix
      //Database.forDataSource(ds)   // TODO fix
    }

    val TM = new DurationInt(5).seconds

    val start = System.currentTimeMillis
    println("getting table list")
    val tables = Await.result(dba.run(MTable.getTables), TM).toList

    println("tables.size: " + tables.size)
    println("Elapsed ms: " + (System.currentTimeMillis - start))
    //tables.map(t => println("    " + t))
  }

}