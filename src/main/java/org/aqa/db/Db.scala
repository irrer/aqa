package org.aqa.db

import slick.backend.DatabaseConfig
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
import org.aqa.Logging._
import java.io.File
import org.aqa.Config
import scala.xml.XML

/** Database utilities. */

object Db {

    /** Ensure that the the configuration has been read. */
    private val configInit = Config.validate

    /**
     * Default timeout for general database overhead operations.  This includes operations like
     *  CREATE or DROP that should happen quickly, but does not include SELECT or other operations that could take longer.
     */
    val TIMEOUT = new DurationInt(5).seconds

    private val dbConfig: DatabaseConfig[PostgresDriver] = DatabaseConfig.forConfig("slick.dbs.default")

    val db = {
        val ds = new ComboPooledDataSource
        ds.setDriverClass(System.getProperty("slick.dbs.default.db.driver")) //  ds.setDriverClass(Driver)
        ds.setJdbcUrl(System.getProperty("slick.dbs.default.db.url")) //   ds.setJdbcUrl(Local)
        ds.setUser(System.getProperty("slick.dbs.default.db.user"))
        ds.setPassword(System.getProperty("slick.dbs.default.db.password"))
        Database.forDataSource(ds)
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
        dbOperation.statements.foreach { s => println("Executing database statement: " + s) }
        run(DBIO.seq(dbOperation))
    }

    /**
     * Determine if table exists in database
     */
    private def tableExists(table: TableQuery[Table[_]]): Boolean = {
        val tables = Await.result(db.run(MTable.getTables), TIMEOUT).toList
        val exists = tables.filter(m => m.name.name.equalsIgnoreCase(tableName(table))).size > 0
        exists
    }

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

}