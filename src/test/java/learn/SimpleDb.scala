package learn

import com.typesafe.config.ConfigFactory
import edu.umro.ScalaUtil.Trace
import java.io.File
import slick.jdbc.JdbcBackend.DatabaseDef
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

//object SimpleDb {
//  def main(args: Array[String]): Unit = {
//    Trace.trace("Starting")
//    val file = new File("""D:\pf\eclipse\workspaceOxygen\aqa\src\test\resources\dbConfig\simpleDb.conf""")
//    val config = ConfigFactory.parseFile(file)
//    //val foo: String = config.getString("simple-lib.foo")
//    //Trace.trace("foo: " + foo)
//    //val stuff = com.typesafe.config.ConfigFactory.load(config)
//    Trace.trace(config.toString.split(",").mkString("\n"))
//
//    trait SmplDb {
//      val config: DatabaseConfig[JdbcProfile]
//      val db: JdbcProfile#Backend#Database = config.db
//
//      case class User(id: Option[Int], email: String,
//        firstName: Option[String], lastName: Option[String])
//
//      case class Address(id: Option[Int], userId: Int,
//        addressLine: String, city: String, postalCode: String)
//
//     // def find2(id: Int) = db.run(User.filter(_.id === id).result.headOption)
//    }
//    
//    class Thing extends SmplDb {
//     // def find(id: Int) = db.run(users.filter(_.id === id).result.headOption)
//
//    }
//
//    val patId = "FooFoo"
//
//    // https://scala-slick.org/doc/3.2.0/api/slick/jdbc/JdbcBackend.html#Database=JdbcBackend.this.DatabaseDef
//    Trace.trace("Done")
//  }
//}


