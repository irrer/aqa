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
import slick.lifted.TableQuery
import Db.driver.api._

//import slick.jdbc.SQLServerProfile

/** Create a clone of the entire database. */

object CloneEntireDB extends Logging {

  val pkClassName = PrimaryKey.getClass.toString

  private def isPrimaryKey(col: FieldSymbol): Boolean = {
    col.options.filter(o => o.getClass.toString.equals(pkClassName)).nonEmpty
  }

  val all: Seq[DbTable] = Seq(Institution)

  /**
   * Get the name of the primary key of the given table.
   */
  private def pkOf(table: TableQuery[Table[_]]): String = {
    val columnList = table.baseTableRow.create_* //.map(_.name)
    val pk = columnList.filter(col => isPrimaryKey(col)).head
    pk.name
  }

  private def foo = {
    Db.createTableIfNonexistent(all.head.tableQuery)
    val row = all.head.getByPk(5)
    all.head.insOrUpdate(row.get)
  }

  /**
   * Get the name of the given table.
   */
  private def nameOf(table: TableQuery[Table[_]]): String = {
    val tableName = table.shaped.shaped.value.tableName
    tableName
  }

  def main(args: Array[String]): Unit = {
    val table = DbSetup.tableQueryList(4)
    val columnList = table.baseTableRow.create_*
    val th = DbSetup.tableQueryList.head.asInstanceOf[TableQuery[Table[_]]]

    val pk = pkOf(th)
    println("pk: " + pk)

    val tableName = nameOf(th)
    val colName = pkOf(th)
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