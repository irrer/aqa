
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.DbAnonymize
import org.aqa.Config
import org.aqa.db.DbSetup

/**
 * Test DbAnonymize.
 *
 */

class TestDbAnonymize extends FlatSpec with Matchers {

  println("Starting...")
  Config.validate
  DbSetup.init

  DbAnonymize.init
  println("Done")
}
