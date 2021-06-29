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

package learn

import slick.driver.PostgresDriver.api._
import com.mchange.v2.c3p0.ComboPooledDataSource
import scala.concurrent.duration.DurationInt
import scala.concurrent.Await
import scala.util.{ Success, Failure }
import scala.concurrent.ExecutionContext.Implicits.global

import scala.reflect.runtime.{ universe => u }

case class AnnotateTable(
  pk: Option[Long], // primary key
  blah: String, // blah's name
  intThingy: Int) {

  def insertOrUpdate = AnnotateTable.run(AnnotateTable.query.insertOrUpdate(this))

  def toName: String = (blah + " " + intThingy).trim
}

object AnnotateTable {
  class AnnotateTableTable(tag: Tag) extends Table[AnnotateTable](tag, "epid") {

    def pk = column[Long]("pk", O.PrimaryKey, O.AutoInc)
    def blah = column[String]("blah")
    def intThingy = column[Int]("intThingy")

    def * = (
      pk.?,
      blah,
      intThingy) <> ((AnnotateTable.apply _)tupled, AnnotateTable.unapply _)
  }

  val query = TableQuery[AnnotateTableTable]

  val TIMEOUT = new DurationInt(60).seconds

  def run[R](op: DBIOAction[R, NoStream, Nothing]): R = {
    val dbAction = db.run(op)
    val result = Await.result(dbAction, TIMEOUT)
    dbAction.onComplete {
      case Failure(ex) => throw (ex)
      case Success(data) => ;
    }
    result
  }

  val db = {
    val ds = new ComboPooledDataSource
    ds.setDriverClass(System.getProperty("slick.dbs.default.db.driver")) //  ds.setDriverClass(Driver)
    ds.setJdbcUrl(System.getProperty("slick.dbs.default.db.url")) //   ds.setJdbcUrl(Local)
    ds.setUser(System.getProperty("slick.dbs.default.db.user"))
    ds.setPassword(System.getProperty("slick.dbs.default.db.password"))
    Database.forDataSource(ds, Some(10))
  }

  @deprecated("hey", "ho")
  def oldybutgoodie(i: Int) = 5

  def main(args: Array[String]): Unit = {
    //def insertOrUpdate = Db.run(AnnotateTable.query.insertOrUpdate(this))
    import java.io.{ FileInputStream, InputStream, File => JFile }

    val jf = new JFile("hey")

    case class Named(name: String) extends scala.annotation.StaticAnnotation

    //import scala.reflect.internal.Symbols.ClassSymbol
//
//    val myAnnotatedClass = u.runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass("MyAnnotatedClass")
//    val annotation: Option[Annotation] = myAnnotatedClass.annotations.find(_.tree.tpe =:= u.typeOf[Named])
//    val result = annotation.flatMap { a =>
//      a.tree.children.tail.collect({ case Literal(Constant(name: String)) => doSomething(name) }).headOption
//    }

  }
}