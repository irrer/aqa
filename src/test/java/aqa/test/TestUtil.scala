
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil

/**
 * Test the Config.
 *
 */

class TestUtil extends FlatSpec with Matchers {

  "randomSecureHash" should "make hash" in {
    (Util.randomSecureHash.size > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Util.randomSecureHash).distinct
    list.size should be(size)
  }
}
