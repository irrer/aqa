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
  barPK: Int, // primary key
  barLong: Long) {
}

object Bar extends Logging {

  class BarTable(tag: Tag) extends Table[Bar](tag, "Bar") {

    def barPK = column[Int]("barPK", O.PrimaryKey, O.AutoInc)
    def barLong = column[Long]("barLong")

    def * = (
      barPK,
      barLong) <> ((Bar.apply _)tupled, Bar.unapply _)
  }

  val query = TableQuery[BarTable]
}
