
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Crypto
import scala.util.Random

/**
 * Test the Config.
 *
 */

class TestUtil_Crypto extends FlatSpec with Matchers {

  val rand = new Random

  "randomSecureHash" should "make hash" in {
    (Crypto.randomSecureHash.size > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Crypto.randomSecureHash).distinct
    list.size should be(size)
  }

  "encrypt" should "be encrypt" in {
    val key = Crypto.makeRandomCipherKey
    val original = "TB_9"
    println("original content size: " + original.size)
    println("message.size: " + original.size)
    val encrypted = Crypto.encrypt(original, key)
    println("encrypted: " + encrypted)
    val decrypted = Crypto.decrypt(encrypted, key)
    println("decrypted message: " + decrypted)

    (decrypted.equals(original)) should be(true)

    // Each time a string is encrypted it should be different.
    val encrypted2 = Crypto.encrypt(original, key)
    (encrypted2.equals(encrypted)) should be(false)

    // encrypt different lengths of strings

    print("Testing string lengths: ")
    for (i <- 0 until 100) {
      val orig = (0 to i).map(ii => rand.nextInt(10)).mkString("")
      print(" " + i)
      val roundTrip = Crypto.decrypt(Crypto.encrypt(orig, key), key)
      (orig.equals(roundTrip)) should be(true)
    }
    println

  }

}
