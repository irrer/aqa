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
import com.microsoft.sqlserver.jdbc.Column
import com.microsoft.sqlserver.jdbc.SQLServerDataSource
import slick.jdbc.SQLServerProfile.api._
import slick.jdbc.meta.MTable
import slick.jdbc.JdbcDataSource
import slick.util.AsyncExecutor
import scala.concurrent.duration.Duration

//import play.db.Database

object ByDataSource extends Logging {

  //  val driver = slick.jdbc.SQLServerProfile
  //  import driver.api._

  def main(args: Array[String]): Unit = {

    val TIMEOUT = new scala.concurrent.duration.DurationInt(5).minutes
    Trace.trace

    try {

      FOO.loadDll
      FOO.findAuthDllJlp
      FOO.findAuthDllPath

      val prop = new Properties
      prop.setProperty("integratedSecurity", "true")
      //prop.setProperty("authenticationScheme", "JavaKerberos")
      //prop.setProperty("integratedSecurity", "false")
      prop.setProperty("user", "UMHS\\irrer")
      prop.setProperty("password", "45eetslp")
      //      prop.setProperty("maxConnection", "5")
      //      prop.setProperty("maxThreads", "5")

      // prop.setProperty("databaseName", "AQAmsDV")
      prop.setProperty("database", "AQAmsDV")

      //  val j =  AsyncExecutor

      val ds = new com.microsoft.sqlserver.jdbc.SQLServerDataSource
      ds.setUser("UMHS\\irrer")
      ds.setIntegratedSecurity(true)
      ds.setPassword("45eetslp")
      ds.setURL("jdbc:sqlserver://ntsrodbsdv1.umhs.med.umich.edu:1433")
      ds.setDatabaseName("AQAmsDV")
      Trace.trace
      //val con = ds.getConnection // prove that we can get a connection
      Trace.trace
      val db = Database.forDataSource(ds, None, AsyncExecutor.default())
      Trace.trace
      val result = FOO.query.result
      Trace.trace
      Trace.trace("statements:\n" + result.statements.mkString("\n    ", "\n    ", "\n    "))
      Trace.trace
      val dbAction = db.run(FOO.query.result)
      Trace.trace
      val out = Await.result(dbAction, TIMEOUT)
      Trace.trace
      dbAction.onComplete {
        case Failure(ex) => throw (ex)
        case Success(data) => ;
      }
      val fooList = out
      Trace.trace("\n    ", fooList.mkString("\n    "))

      if (false) {
        val tableList = db.run(MTable.getTables)
        Trace.trace
        Trace.trace(tableList)
        Thread.sleep(1000)
        Trace.trace(tableList)
      }

      Trace.trace
      db.run(MTable.getTables(Some(""), Some("public"), Some(""), Some(Seq("TABLE")))).
        onComplete {
          case Success(tables) => Trace.trace("\n    " + (tables.map(_.name).mkString(" - ")))
          case Failure(f) => Trace.trace("\n    " + f)
        }

      Thread.sleep(1000)

      Trace.trace
      //      val drv = slick.jdbc.SQLServerProfile
      //      val j = slick.driver.SQLServer
      //
      //            Await.result(db.run(db.driver.defaultTables), Duration.Inf).foreach(println)
      //            Trace.trace
      //      val tables = Await.result(db.run(MTable.getTables), TIMEOUT).toList
      //      Trace.trace
      //      val slickDataSource = slick.jdbc.JdbcDataSourceFactory
      //
      //     val db = Database.forSource(sds)

      //      Trace.trace("getting list of tables")
      //      val tables = Await.result(db.run(MTable.getTables), TIMEOUT).toList
      //      Trace.trace
      //      tables.map(m => println("Found table: " + m.name.name))
      //      Trace.trace
      //
      //      Trace.trace("getting list of FOOs")
      //      val dbAction = db.run(FOO.query.result)
      //
      //      Trace.trace("awaiting result")
      //      val result = Await.result(dbAction, TIMEOUT)
      //      Trace.trace("!!!!!!!!!!!!!!!!!!!!!!!!!!!!! after result    result.isEmpty: " + result.isEmpty)
      //      dbAction.onComplete {
      //        case Failure(ex) => throw (ex)
      //        case Success(data) => Trace.trace("!!!!!!!!!!!!!!!!!!!!!!!!!!!!! data: " + data)
      //      }
      //      Trace.trace(result.mkString("\n"))
      Trace.trace("success!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    } catch {
      case t: Throwable => {
        println("\n\nbadness: " + t)
      }
    }
    println("done")
    System.exit(99)

  }
}
