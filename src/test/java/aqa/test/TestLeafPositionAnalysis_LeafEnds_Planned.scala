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

package aqa.test

import org.aqa.Util
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test the LeafPositionAnalysis.leafEnds method.
  */

class TestLeafPositionAnalysis_LeafEnds_Planned extends FlatSpec with Matchers {
  "LeafPositionAnalysis" should "show leaf end positions in isoplan in mm" in {
    val dir = new File("""src\test\resources\TestLeafPositionAnalysis_LeafEnds_Planned""")

    val planFile = new File(dir, """rtplan1.dcm""")
    println("Using DICOM RTPLAN file " + planFile.getAbsolutePath)
    val planAl = Util.readDicomFile(planFile).right.get
    val leafEndList = LeafPositionUtil.leafEnds(horizontal = true, "PF Stat 0", planAl)

    // @formatter:off
    val expectedLeafEndList = Seq(
      -59.5,
      -44.5,
      -29.5,
      -14.5,
        0.5,
       15.5,
       30.5,
       45.5,
       60.5,
       75.5)
    // @formatter:on

    println("list of leaf ends:\n    " + leafEndList.map(_.formatted("%5.1f")).mkString("\n    "))

    leafEndList should be(expectedLeafEndList)
  }
}
