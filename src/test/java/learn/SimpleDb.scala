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


