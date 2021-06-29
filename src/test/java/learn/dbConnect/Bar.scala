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

package learn.dbConnect

//import org.aqa.db.Db.driver.api._
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import scala.concurrent.Await
import com.typesafe.config.ConfigFactory
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import java.sql.DriverManager
import java.util.Properties
import java.io.File
import slick.util.ClassLoaderUtil
import javax.sql.DataSource
import slick.lifted.ProvenShape.proveShapeOf
import com.microsoft.sqlserver.jdbc.SQLServerDriver
import slick.jdbc.SQLServerProfile.api._

case class Bar(
  barPK: Option[Int], // primary key
  barLong: Long,
  barString: Option[String]) {
}

object Bar extends Logging {

  class BarTable(tag: Tag) extends Table[Bar](tag, "Bar") {

    def barPK = column[Option[Int]]("barPK", O.PrimaryKey, O.AutoInc)
    def barLong = column[Long]("barLong")
    def barString = column[Option[String]]("barString")

    def * = (
      barPK,
      barLong,
      barString) <> ((Bar.apply _)tupled, Bar.unapply _)
  }

  val query = TableQuery[BarTable]
  
  
  

  
}
