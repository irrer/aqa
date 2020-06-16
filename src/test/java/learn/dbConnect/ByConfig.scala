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

  val dbConfigText = """
        db {
          default = {
            driver = "slick.jdbc.SQLServerProfile$"
            db {
                serverName = "ntsrodbsdv1.umhs.med.umich.edu"
                portNumber = "1433"
                databaseName = "AQAmsDV"
                connectionPool = disabled
                url = "jdbc:sqlserver://ntsrodbsdv1.umhs.med.umich.edu:1433"
                user = "irrer"
                password = "45eetslp"
                integratedSecurity = true
            }
          }
          numThreads = 10
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
