package learn

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import scala.concurrent.Await
import com.typesafe.config.ConfigFactory
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import com.mchange.v2.c3p0.ComboPooledDataSource
import slick.sql.FixedSqlAction
import java.sql.DriverManager
import java.util.Properties
import com.microsoft.sqlserver.jdbc.Column
import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement
import java.util.concurrent.Semaphore

case class FOO(
  PK: Int, // primary key
  TEXT: String) {
}

object FOO extends Logging {

  val driver = slick.jdbc.SQLServerProfile
  import driver.api._

  class FOOTable(tag: Tag) extends Table[FOO](tag, "epid") {

    def PK = column[Int]("PK", O.PrimaryKey, O.AutoInc)
    def TEXT = column[String]("TEXT")

    def * = (
      PK,
      TEXT) <> ((FOO.apply _)tupled, FOO.unapply _)
  }

  val query = TableQuery[FOOTable]

  val refIt = """
<!--
              url = "jdbc:sqlserver://NTSRODBSDV1:1433/AQAmsDV;trustServerCertificate=true;integratedSecurity=true;authenticationScheme=NativeAuthentication"
-->
<!-- 
          default = {
            driver = "slick.jdbc.SQLServerProfile$"
            db {
              host = NTSRODBSDV1
              port = 1433
              databaseName = AQAmsDV
              DatabaseName = AQAmsDV
              user = irrer
              password = 45eetslp
              XconnectionPool = disabled
              driver = "slick.jdbc.SQLServerProfile$"
              profile = "slick.jdbc.SQLServerProfile$"
              url = "jdbc:sqlserver://10.30.3.65:1433;servername=NTSRODBSDV1;DatabaseName=AQAmsDV;trustServerCertificate=true;integratedSecurity=true;authenticationScheme=NativeAuthentication"
              properties = {
                driver = "slick.jdbc.SQLServerProfile$"
                profile = "slick.jdbc.SQLServerProfile$"
                user = irrer
                password = 45eetslp
                port = 1433
                host = 10.30.3.65
                databaseName = AQAmsDV
                numThreads = 10
                trustServerCertificate = true
                integratedSecurity = true
                authenticationScheme = NativeAuthentication
              }
            }
          }
-->

           //  connectionPool = "HikariCP" //use HikariCP for our connection pool
           //  dataSourceClass = "org.postgresql.ds.PGSimpleDataSource" //Simple datasource with no connection pooling. The connection pool has already been specified with HikariCP.

                integratedSecurity = true


"""

  val prefix = "db.default.db"

  val dbConfigText = """
        db {
          default = {
            driver = "slick.jdbc.SQLServerProfile$"
            db {
                serverName = "NTSRODBSDV1.UMHS.MED.UMICH.EDU"
                portNumber = "1433"
                databaseName = "AQAmsDV"
                connectionPool = disabled
                url = "jdbc:sqlserver://NTSRODBSDV1.UMHS.MED.UMICH.EDU:1433"
                user = "irrer"
                password = "45eetslp"
                trustServerCertificate = true
            }
          }
          numThreads = 10
        }
"""

  val dbConfig = ConfigFactory.parseString(dbConfigText)

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

    try {

      if (true) {
        Trace.trace("starting setup")
        val jlp = "java.library.path"
        Trace.trace(jlp + ": " + System.getProperty(jlp))
        val p = System.getProperty(jlp)
        val pp = """D:\pf\eclipse\workspaceOxygen\aqa\src\main\dll;""" + p
        //val pp = """D:\pf\eclipse\workspaceOxygen\aqa\src\main\dll\sqljdbc_auth.dll;""" + p
        System.setProperty(jlp, pp)
        Trace.trace(jlp + ": " + System.getProperty(jlp))
        val dll = """D:\pf\eclipse\workspaceOxygen\aqa\src\main\dll\sqljdbc_auth.dll"""
        System.load(dll)
        Trace.trace("Loaded " + dll)
      }

      if (true) {
        import java.sql.Connection
        import java.sql.DriverManager
        import com.microsoft.sqlserver.jdbc.Column
        val properties = new Properties
        properties.setProperty("integratedSecurity", "true")
        //properties.setProperty("applicationIntent", "ReadOnly")
        properties.setProperty("user", "UMHS\\irrer")
        properties.setProperty("password", "45eetslp")
        properties.setProperty("databaseName", "AQAmsDV")
        Trace.trace(properties)
        //val url = "jdbc:sqlserver://10.30.3.65:1433"
        val url = "jdbc:sqlserver://NTSRODBSDV1.UMHS.MED.UMICH.EDU:1433"
        Trace.trace("\nurl: " + url + "\n" + properties.keySet.toArray.map(k => "    " + k.toString + " : " + properties.getProperty(k.toString)).mkString("\n"))
        Trace.trace("Getting connection ...")
        val connection = DriverManager.getConnection(url, properties)
        Trace.trace("Got a connection")
        Trace.trace(connection.getMetaData)
      }

      val db = configureDb
      Trace.trace("getting list of FOOs")
      val dbAction = db.run(query.result)
      Trace.trace("awaiting result")
      val result = Await.result(dbAction, TIMEOUT)
      Trace.trace("after result    result.isEmpty: " + result.isEmpty)
      dbAction.onComplete {
        case Failure(ex) => throw (ex)
        case Success(data) => ;
      }
      Trace.trace(result.mkString("\n"))
    } catch {
      case t: Throwable => {
        Trace.trace("\n\nbadness: " + fmtEx(t))
      }
    }
    System.exit(99)

  }
}
