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
import slick.ast.ColumnOption
import slick.ast.ColumnOption.PrimaryKey
import slick.model.PrimaryKeyOption
import slick.ast.FieldSymbol

//import slick.jdbc.SQLServerProfile

/** Create a clone of the entire database. */

object CloneEntireDB extends Logging {

  val pkClassName = PrimaryKey.getClass.toString

  private def isPrimaryKey(col: FieldSymbol): Boolean = {
    col.options.filter(o => o.getClass.toString.equals(pkClassName)).nonEmpty
  }

  //  object DestDb {
  //    import slick.lifted.TableQuery
  //
  //    import slick.jdbc.SQLServerProfile.api._
  //    val destDb: Database = null
  //  }

  object SourceDb {
    import slick.lifted.TableQuery
    import Db.driver.api._

    val j = Institution.query
    val all: Seq[DbTable] = Seq(Institution)

    /**
     * Get the name of the primary key of the given table.
     */
    def pkOf(table: TableQuery[Table[_]]): String = {
      val columnList = table.baseTableRow.create_* //.map(_.name)
      val pk = columnList.filter(col => isPrimaryKey(col)).head
      pk.name
    }

    private def foo = {
      Db.createTableIfNonexistent(all.head.tableQuery)
      val row = all.head.getByPk(5, Db.db)
      all.head.insOrUpdate(row.get)
    }

    /**
     * Get the name of the given table.
     */
    def nameOf(table: TableQuery[Table[_]]): String = {
      val tableName = table.shaped.shaped.value.tableName
      tableName
    }

  }

  def main(args: Array[String]): Unit = {
    import slick.lifted.TableQuery
    import Db.driver.api._

    val dbSetup = DbSetup.init
    Trace.trace

    val table = DbSetup.getTable(0)
    val columnList = table.baseTableRow.create_*
    val th = DbSetup.getTable.head.asInstanceOf[TableQuery[Table[_]]]

    if (false) {
      val dbTable = Institution.asInstanceOf[DbTable]
      val oldRow = dbTable.getByPk(4, Db.db)
      //dbTable.tableQuery += oldRow.get
    }

    if (false) { // works
      val badPk: Long = 23
      val goodPk: Long = 1000
      val action = Institution.query.filter(_.institutionPK === badPk).map(i => i.institutionPK).update(goodPk)
      val result = Db.run(action)
      println("result: " + result)
    }

    if (true) { // works
      val dbTable = Institution.asInstanceOf[DbTable]
      println("tableName: " + dbTable.tableName)
      println("query: " + dbTable.query)
      println("queryT: " + dbTable.query.asInstanceOf[TableQuery[Table[_]]])
      val oldRow = dbTable.getByPk(4, Db.db)
      println("oldRow: " + oldRow)
      val inst = oldRow.get.asInstanceOf[Institution]
      val newInst = inst.copy(institutionPK = Some(1001))
      val newRow = newInst.asInstanceOf[Any]
      val action = dbTable.insOrUpdate(newRow)
      action.statements.toList.map(s => println("statement: " + s))
      val result = Db.run(action)
      println("result: " + result)
    }

    val pk = SourceDb.pkOf(th)
    println("pk: " + pk)

    val tableName = SourceDb.nameOf(th)
    val colName = SourceDb.pkOf(th)
    val action = sql"""select "#$colName" from "#$tableName"""".as[(Long)]

    val result = Db.run(action)
    action.statements.foreach(println)
    result.foreach(println)

    //    val j1 = th.filter(x => true)
    //
    //    val j2 = th.filter(_.indexes => true)
    //
    //    val j3 = th.filter(x => pk == 1)

  }

}