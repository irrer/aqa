package learn

//import scala.language.higherKinds

//import slick.driver.PostgresDriver.api.ColumnType
//import slick.driver.PostgresDriver.api.columnExtensionMethods
//import slick.driver.PostgresDriver.api.ColumnsShapeLevel
//import slick.driver.PostgresDriver.api.Table
//import slick.driver.PostgresDriver.api.TableQuery
//import slick.driver.PostgresDriver.api.ColumnType
//import slick.driver.PostgresDriver.api.charColumnType
//import slick.driver.PostgresDriver.api.clobColumnType
//import slick.driver.PostgresDriver.api.columnToOptionColumn
//import slick.driver.PostgresDriver.api.columnToOrdered
//import slick.driver.PostgresDriver.api.Tag

//import slick.driver.JdbcProfile
//import slick.driver.JdbcDriver
//import slick.jdbc._

//import slick.backend.DatabaseConfig
//import slick.driver.JdbcProfile

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.db.slick.DatabaseConfigProvider
import slick.ast.BaseTypedType
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

//import io.strongtyped.active.slick{ ActiveRecord, EntityActions , ModelIdContract }
import io.strongtyped.active.slick.ActiveRecord
import io.strongtyped.active.slick.EntityActions
//import io.strongtyped.active.slick.ModelIdContract

//import config.driver.api._

//case class AgnosticTable(
//  pk: Option[Long], // primary key
//  blah: String, // blah's name
//  intThingy: Int) {
//
//  //def insertOrUpdate = Db.run(AgnosticTable.query.insertOrUpdate(this))
//
//  def toName: String = (blah + " " + intThingy).trim
//}
//
//object AgnosticTable {
//  class AgnosticTableTable(tag: Tag) extends Table[AgnosticTable](tag, "epid") {
//
//    def pk = column[Long]("pk", O.PrimaryKey, O.AutoInc)
//    def blah = column[String]("blah")
//    def intThingy = column[Int]("intThingy")
//
//    def * = (
//      pk.?,
//      blah,
//      intThingy) <> ((AgnosticTable.apply _)tupled, AgnosticTable.unapply _)
//  }
//
//  val query = TableQuery[AgnosticTableTable]
//
//}