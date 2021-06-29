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

import scala.language.higherKinds
import slick.driver.JdbcProfile

/**
 * All database code goes into the DAO (data access object) class which
 * is parameterized by a Slick driver that implements JdbScProfile.
 */

import FooFoo.driver.api._

//val j = FooFoo.driver.api.Case

class Props(tag: Tag) extends Table[(String, String)](tag, "Props") {
  def key = column[String]("KEY", O.PrimaryKey)
  def value = column[String]("VALUE")
  def * = (key, value)
}

object JimDAO {
  val props = TableQuery[Props]

  /** Create the database schema */
  def create: DBIO[Unit] = props.schema.create
  //    props.ddl.create

  /** Insert a key/value pair */
  def insert(k: String, v: String): DBIO[Int] =
    props += (k, v)

  /** Get the value for the given key */
  def get(k: String): DBIO[Option[String]] =
    (for (p <- props if p.key === k) yield p.value).result.headOption

  /** Get the first element for a Query from this DAO */
  def getFirst[M, U, C[_]](q: Query[M, U, C]): DBIO[U] =
    q.result.head

  def main(args: Array[String]): Unit = {

    //val jimDao = new JimDAO(H2Driver)
    //    import dao.driver.api._
    //    dao.run(dao.insert("foo", "bar"))  // need this but it will not compile
  }
}