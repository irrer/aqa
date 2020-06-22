package org.aqa.db

//import scala.concurrent.duration.DurationInt
//import slick.jdbc.meta.MTable
//import scala.concurrent.Await
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.util.{ Success, Failure }
//import com.mchange.v2.c3p0.ComboPooledDataSource
//import scala.xml.Elem
//import org.aqa.run.ProcedureStatus
//import org.aqa.Logging
//import java.io.File
//import org.aqa.Config
//import scala.xml.XML
//import edu.umro.ScalaUtil.Trace
//import com.typesafe.config.ConfigFactory
import slick.sql.FixedSqlAction
import Db.driver.api._

trait DbTable {
  val tableQuery: TableQuery[Table[_]];
  def getByPk(pk: Long): Option[Any];
  def insOrUpdate(row: Any): FixedSqlAction[Int, slick.dbio.NoStream, slick.dbio.Effect.Write]

  val tableName: String = tableQuery.shaped.shaped.value.tableName

}