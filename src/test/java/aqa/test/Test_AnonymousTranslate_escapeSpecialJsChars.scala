
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
