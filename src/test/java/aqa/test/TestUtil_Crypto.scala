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
import org.aqa.AnonymizeUtil

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
    val encrypted = Crypto.encryptWithNonce(original, key)
    println("encrypted: " + encrypted)
    val decrypted = Crypto.decryptWithNonce(encrypted, key)
    println("decrypted message: " + decrypted)

    (decrypted.equals(original)) should be(true)

    // Each time a string is encrypted it should be different.
    val encrypted2 = Crypto.encryptWithNonce(original, key)
    (encrypted2.equals(encrypted)) should be(false)

    // encrypt different lengths of strings

    print("Testing string lengths: ")
    for (i <- 0 until 100) {
      val orig = (0 to i).map(ii => rand.nextInt(10)).mkString("")
      print(" " + i)
      val roundTrip = Crypto.decryptWithNonce(Crypto.encryptWithNonce(orig, key), key)
      (orig.equals(roundTrip)) should be(true)
    }
    println

  }

  AnonymizeUtil.aliasify("HEY", 4) should be("HEY___4")
  AnonymizeUtil.aliasify("HEY", 56) should be("HEY__56")
  AnonymizeUtil.aliasify("HO", 123456) should be("HO123456")
  AnonymizeUtil.aliasify("HO", 3456) should be("HO3456")

}
