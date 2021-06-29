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
import slick.jdbc.SQLServerProfile.api._

object ByConfig extends Logging {

  //  val driver = slick.jdbc.SQLServerProfile
  //  import driver.api._

  val prefix = "db.default.db"

  //slick.jdbc.SQLServerProfile
  val ds = new com.microsoft.sqlserver.jdbc.SQLServerDataSource
  Trace.trace(ds.getClass)
  // System.exit(99)
  //     driver = "slick.jdbc.SQLServerProfile$"
  //                connectionPool = disabled
  //           numThreads = 10
  //            driver = "slick.jdbc.SQLServerProfile$"

  val dbConfigText = """
        db {
          default = {
            driver = "com.typesafe.slick.driver.ms.SQLServerDriver"
            db {
                driver = "com.microsoft.sqlserver.jdbc.SQLServerDriver"
                serverName = "ntsrodbsdv1.umhs.med.umich.edu"
                portNumber = "1433"
                databaseName = "AQAmsDV"
                url = "jdbc:sqlserver://ntsrodbsdv1.umhs.med.umich.edu:1433"
                user = "irrer"
                password = "45eetslp"
                integratedSecurity = true
            }
          }
        }
"""

  //              trustServerCertificate = true

  val dbConfig = ConfigFactory.parseString(dbConfigText)

  Trace.trace(dbConfig.getString("db.default.driver"))
  Trace.trace(dbConfig.getString("db.default.db.serverName"))
  //Trace.trace(dbConfig.getString("db.default.db.trustServerCertificate"))
  Trace.trace(dbConfig.getString("db.default.db.integratedSecurity"))
  //System.exit(99)

  def configureDb = {
    val prefix = "db.default.db"
    Trace.trace("Attempting to configure\n" + dbConfigText)

    // create the database from the config
    val dd = Database.forConfig(prefix, dbConfig)
    Trace.trace("Was able to configure database")
    dd
  }

  def main(args: Array[String]): Unit = {

    val TIMEOUT = new scala.concurrent.duration.DurationInt(5).minutes
    Trace.trace
    FOO.findAuthDllJlp
    FOO.findAuthDllPath

    try {

      val db = configureDb
      Trace.trace
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
        Trace.trace("\n\nbadness: " + t)
      }
    }
    System.exit(99)

  }
}
