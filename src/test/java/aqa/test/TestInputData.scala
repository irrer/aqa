
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.InputData
import org.aqa.db.DbSetup

/**
 * Test InputData.
 *
 */

class TestInputData extends FlatSpec with Matchers {

  DbSetup.init

  "insert" should "add entry" in {

    if (true) {
      val pk: Long = 40 // TODO should reference test input
      val inputData = new InputData(pk, pk, Array[Byte](2, 3, 5, 7, 11))
      Trace.trace("inputData: " + inputData)
      inputData.insert

      val inputData2 = InputData.get(pk)

      (inputData2.isDefined) should be(true)

      inputData2.get.inputDataPK should be(pk)
      inputData2.get.inputPK should be(pk)

      (inputData.data.toList == inputData2.get.data.toList) should be(true)

      InputData.delete(pk)

      (InputData.get(pk).isEmpty) should be(true)
    }
    true should be(true)
  }

}
