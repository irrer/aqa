
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Test the Config.
 *
 */

class TestConfig extends FlatSpec with Matchers {
  Config.validate
  "Configuration" should "define values" in {

    Config.validated should be(true)
    Config.TermsOfUse.nonEmpty should be(true)
  }
}
