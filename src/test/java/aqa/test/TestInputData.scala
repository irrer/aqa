
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.InputFiles
import org.aqa.db.DbSetup

/**
 * Test InputFiles.
 *
 */

class TestInputFiles extends FlatSpec with Matchers {

  DbSetup.init

  "insert" should "add entry" in {

    if (true) {
      val pk: Long = 40 // TODO should reference test input
      val inputFiles = new InputFiles(pk, pk, Array[Byte](2, 3, 5, 7, 11))
      Trace.trace("inputFiles: " + inputFiles)
      inputFiles.insert

      val inputFiles2 = InputFiles.get(pk)

      (inputFiles2.isDefined) should be(true)

      inputFiles2.get.inputFilesPK should be(pk)
      inputFiles2.get.inputPK should be(pk)

      (inputFiles.zippedContent.toList == inputFiles2.get.zippedContent.toList) should be(true)

      InputFiles.delete(pk)

      (InputFiles.get(pk).isEmpty) should be(true)
    }
    true should be(true)
  }

}
