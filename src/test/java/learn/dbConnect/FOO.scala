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

case class FOO(
  PK: Int, // primary key
  TEXT: String) {
}

object FOO extends Logging {

  class FOOTable(tag: Tag) extends Table[FOO](tag, "FOO") {

    def PK = column[Int]("PK", O.PrimaryKey, O.AutoInc)
    def TEXT = column[String]("TEXT")

    def * = (
      PK,
      TEXT) <> ((FOO.apply _)tupled, FOO.unapply _)
  }

  val query = TableQuery[FOOTable]

  // ---- Utilities ----

  val dllFileName = "sqljdbc_auth.dll"
  val jlpName = "java.library.path"
  def jlpToString = System.getProperty(jlpName).split(";").mkString("\n    ", "\n    ", "")

  def findAuthDllJlp = {
    System.getProperty(jlpName).split(";").map(dn => new File(dn)).map(d => new File(d, dllFileName)).filter(f => f.canRead).map(f => println("   jlp  found dll: " + f.getAbsolutePath))
  }

  def findAuthDllPath = {
    System.getenv("path").split(";").map(dn => new File(dn)).map(d => new File(d, dllFileName)).filter(f => f.canRead).map(f => println("   path found dll: " + f.getAbsolutePath))
  }

  val dllFullPath = """D:\pf\cygwin\bin\sqljdbc_auth.dll"""

  def loadDll = {
    println("starting loadDll")
    System.load(dllFullPath)
    println("Loaded " + dllFullPath)
  }

}
