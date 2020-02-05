package learn

import scala.language.higherKinds
import slick.driver.JdbcProfile

object FooFoo {
  import slick.driver.H2Driver
  import slick.driver.SQLiteDriver
  import slick.driver.PostgresDriver

  val driver = slick.driver.PostgresDriver

}
