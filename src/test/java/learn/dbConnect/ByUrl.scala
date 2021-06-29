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
import java.sql.Connection
import java.sql.DriverManager
import com.microsoft.sqlserver.jdbc.Column
import com.microsoft.sqlserver.jdbc.SQLServerDataSource
import slick.jdbc.SQLServerProfile.api._
import slick.jdbc.meta.MTable
//import play.db.Database

object ByUrl extends Logging {

  //  val driver = slick.jdbc.SQLServerProfile
  //  import driver.api._

  def main(args: Array[String]): Unit = {

    val TIMEOUT = new scala.concurrent.duration.DurationInt(5).minutes
    Trace.trace

    try {

      val prop = new Properties
      prop.setProperty("integratedSecurity", "true")
      prop.setProperty("authenticationScheme", "JavaKerberos")
      //prop.setProperty("integratedSecurity", "false")
      prop.setProperty("user", "UMHS\\irrer")
      prop.setProperty("password", "45eetslp")
      //      prop.setProperty("maxConnection", "5")
      //      prop.setProperty("maxThreads", "5")

      // prop.setProperty("databaseName", "AQAmsDV")
      prop.setProperty("database", "AQAmsDV")

      val db = Database.forURL(
        url = "jdbc:sqlserver://ntsrodbsdv1.umhs.med.umich.edu:1433",
        user = "UMHS\\irrer",
        password = "45eetslp",
        prop = prop,
        driver = "com.microsoft.sqlserver.jdbc.SQLServerDriver"
      //executor = AsyncExecutor.default(),
      //keepAliveConnection = false,
      //classLoader = ClassLoaderUtil.defaultClassLoader
      )

      Trace.trace("getting list of tables")
      val tables = Await.result(db.run(MTable.getTables), TIMEOUT).toList
      Trace.trace
      tables.map(m => println("Found table: " + m.name.name))
      Trace.trace

      Trace.trace("getting list of FOOs")
      val dbAction = db.run(FOO.query.result)

      Trace.trace("awaiting result")
      val result = Await.result(dbAction, TIMEOUT)
      Trace.trace("!!!!!!!!!!!!!!!!!!!!!!!!!!!!! after result    result.isEmpty: " + result.isEmpty)
      dbAction.onComplete {
        case Failure(ex) => throw (ex)
        case Success(data) => Trace.trace("!!!!!!!!!!!!!!!!!!!!!!!!!!!!! data: " + data)
      }
      Trace.trace(result.mkString("\n"))

    } catch {
      case t: Throwable => {
        println("\n\nbadness: " + t)
      }
    }
    println("done")
    System.exit(99)

  }
}
