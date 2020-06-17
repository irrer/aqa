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
import java.util.Date

//import play.db.Database

object ByDataSource extends Logging {

  //  val driver = slick.jdbc.SQLServerProfile
  //  import driver.api._

  def main(args: Array[String]): Unit = {

    val TIMEOUT = new scala.concurrent.duration.DurationInt(5).minutes
    Trace.trace

    try {

      //      FOO.loadDll
      //      FOO.findAuthDllJlp
      //      FOO.findAuthDllPath

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
      val con = ds.getConnection // prove that we can get a connection
      Trace.trace
      val db = Database.forDataSource(ds, None, AsyncExecutor.default())
      val dbTyped: Database = db
      Trace.trace("db.getClass: " + db.getClass)
      Trace.trace("dbTyped.getClass: " + dbTyped.getClass)
      Trace.trace

      def run[R](op: DBIOAction[R, NoStream, Nothing]): R = {
        val dbAction = db.run(op)
        val result = Await.result(dbAction, TIMEOUT)
        dbAction.onComplete {
          case Failure(ex) => Trace.trace("database failure: " + ex)
          case Success(data) => ;
        }
        result
      }

      if (false) { // this works
        val result = FOO.query.result
        Trace.trace
        Trace.trace("statements:\n" + result.statements.mkString("\n    ", "\n    ", "\n    "))
        Trace.trace
        val dbAction = db.run(FOO.query.result)
        Trace.trace
        val out = Await.result(dbAction, TIMEOUT)
        Trace.trace
      }

      if (false) { // this works
        Trace.trace
        val dbOperation = Bar.query.schema.drop

        dbOperation.statements.foreach { s => logger.info("Executing database statement: " + s) }

        val dbAction = db.run(dbOperation)
        val j = Await.result(dbAction, TIMEOUT)
        Thread.sleep(1000)
        Trace.trace
      }

      if (false) { // this works
        Trace.trace
        val dbOperation = Bar.query.schema.create

        dbOperation.statements.foreach { s => logger.info("Executing database statement: " + s) }

        val dbAction = db.run(dbOperation)
        val j = Await.result(dbAction, TIMEOUT)
        Thread.sleep(1000)
        Trace.trace
      }

      if (false) { // this works
        Trace.trace
        val action = sql"select TABLE_NAME from AQAmsDV.INFORMATION_SCHEMA.TABLES".as[String]
        val dbAction = db.run(action)
        Thread.sleep(1000)
        val list = Await.result(dbAction, TIMEOUT)

        Trace.trace("List of tables: \n    " + list.mkString("\n    "))

        Trace.trace
      }

      if (false) {
        val tableList = db.run(MTable.getTables)
        Trace.trace
        Trace.trace(tableList)
        Thread.sleep(1000)
        Trace.trace(tableList)
        Trace.trace
      }

      if (false) {
        Trace.trace
        val action = MTable.getTables(Some(""), Some("public"), Some(""), Some(Seq("TABLE")))
        db.run(MTable.getTables(Some(""), Some("public"), Some(""), Some(Seq("TABLE")))).
          onComplete {
            case Success(tables) => Trace.trace("\n    " + (tables.map(_.name).mkString(" - ")))
            case Failure(f) => Trace.trace("\n    " + f)
          }
        Thread.sleep(1000)
        Trace.trace
      }

      if (false) {

        val foo1 = new FOO(2, "ups 2 1")
        val result1 = run(FOO.query.insertOrUpdate(foo1))
        Trace.trace(result1)

        val foo2 = new FOO(3, "ups 3 1")
        val result2 = run(FOO.query.insertOrUpdate(foo2))
        Trace.trace(result2)

        Trace.trace
      }

      if (false) { // this works
        val bar = new Bar(None, System.currentTimeMillis % 1000, Some("barry"))

        val action = Bar.query += bar
        db.run(action)
        Thread.sleep(1000)
      }

      if (false) { // this works
        val bar = new Bar(None, System.currentTimeMillis % 1000, Some("barIns"))
        val insertQuery = Bar.query returning Bar.query.map(_.barPK) into ((input, barPK) => input.copy(barPK = barPK))
        val actionI = insertQuery += bar
        val resultI = run(actionI)
        Trace.trace(resultI)
      }

      if (true) {
        val bar = new Bar(None, System.currentTimeMillis % 1000, Some("barUpsrtNone"))
        val action = Bar.query.insertOrUpdate(bar)
        val result = Await.result(db.run(action), Duration.Inf)
        Trace.trace("statements:\n    " + result.result.statements.mkString("\n    "))
        Trace.trace(result)
        Trace.trace
      }

      Trace.trace

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
