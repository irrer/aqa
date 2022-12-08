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
import org.aqa.web.AnonymousTranslate

/**
 * Test Test_AnonymousTranslate_escapeSpecialJsChars.
 *
 */

class Test_AnonymousTranslate_escapeSpecialJsChars extends FlatSpec with Matchers {

  "strings" should "match" in {

    val data = Seq(
      ("hey there", "hey there"),
      ("hi\nthere", "hi\\nthere"),
      ("ho\\there", "ho\\\\there"))

    for (pair <- data) {
      val before = pair._1
      val expected = pair._2
      val after = AnonymousTranslate.escapeSpecialJsChars(before)
      println(
        "\n\nbefore: >>" + before + "<<" +
          "    expected >>" + expected + "<<" +
          "    after >>" + after + "<<" + "\n")

      expected should be(after)
    }

  }

}
