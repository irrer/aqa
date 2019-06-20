
package aqa.test;

import org.aqa.Config
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqa.Crypto

/**
 * Test Crypto.hexToByteArray
 *
 */

class TestCryptoHex extends FlatSpec with Matchers {
  "crytpo hex" should "be good hex" in {

    val text = Seq("01", "23", "ab", "AB")
    val good = Crypto.hexToByteArray(text.mkString(""))
    val bad = Crypto.hexToByteArray(text.mkString("") + "44")

    val answer = text.map(t => Integer.parseInt(t, 16).toByte)

    answer should be(good)
    answer shouldNot be(bad)

  }
}
