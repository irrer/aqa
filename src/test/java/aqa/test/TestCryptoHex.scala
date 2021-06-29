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
