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
import org.aqa.db.InputFiles
import org.aqa.db.DbSetup

/**
 * Test InputFiles.
 *
 */

class TestInputFiles extends FlatSpec with Matchers {

  DbSetup.init

  "insert" should "add entry" in {

    val pk: Long = 1000 * 1000 * 1000 // TODO should reference test input

    if (false) {
      val inputFiles = new InputFiles(pk, pk, Array[Byte](2, 3, 5, 7, 11))
      Trace.trace("inputFiles: " + inputFiles)
      inputFiles.insert

      val inputFiles2 = InputFiles.get(pk)

      (inputFiles2.isDefined) should be(true)

      inputFiles2.get.inputFilesPK should be(pk)
      inputFiles2.get.inputPK should be(pk)

      (inputFiles.zippedContent.toList == inputFiles2.get.zippedContent.toList) should be(true)

      InputFiles.delete(pk)
    }

    (InputFiles.get(pk).isEmpty) should be(true)
  }

}
