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
import java.io.File
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import org.aqa.webrun.phase2.leafPosition.LeafPositionUtil

/**
 * Test the LeafPositionAnalysis.leafEnds method.
 */

class TestLeafPositionAnalysis_LeafEnds_Planned extends FlatSpec with Matchers {
  "LeafPositionAnalysis" should "show leaf end positions in isoplan in mm" in {
    val dir = new File("""src\test\resources""")

    val planFile = new File(dir, """TestLeafPositionAnalysisPlan.dcm""")
    val planAl = Util.readDicomFile(planFile).right.get
    val leafEndList = LeafPositionUtil.leafEnds(true, "PF Stat 0", planAl)

    val expectedLeafEndList = Seq(
      -60.0,
      -45.0,
      -30.0,
      -15.0,
      0.0,
      15.0,
      30.0,
      45.0,
      60.0,
      75.0)

    println("list of leaf ends:\n    " + leafEndList.mkString("\n    "))

    leafEndList should be(expectedLeafEndList)
  }
}
