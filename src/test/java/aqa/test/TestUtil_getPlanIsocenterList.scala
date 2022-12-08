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
import edu.umro.ScalaUtil.Trace
import java.io.File
import org.aqa.DicomFile
import com.pixelmed.dicom.AttributeList

/**
 * Test the Util.getPlanIsocenterList
 */

class TestUtil_getPlanIsocenterList extends FlatSpec with Matchers {

  val dir = new File("""src\test\resources\TestUtil_getPlanIsocenterList""")
  println("Using input directory " + dir.getAbsolutePath)

  val rtplanFileList = dir.listFiles.map(f => (new DicomFile(f))).toSeq // (new DicomFile(f)).attributeList.get)

  "getPlanIsocenterList" should "find isocenters defined in plan" in {

    def testIt(rtplan: DicomFile) {
      val isocenterList = Util.getPlanIsocenterList(rtplan.attributeList.get)

      println("Plan file: " + rtplan.file.getAbsolutePath)
      println("Isocenter list:\n    " + isocenterList.map(p => Util.fmtDbl(p.getX) + ", " + Util.fmtDbl(p.getY) + ", " + Util.fmtDbl(p.getZ)).mkString("\n    "))
      println("Distinct list:\n    " + isocenterList.distinct.map(p => Util.fmtDbl(p.getX) + ", " + Util.fmtDbl(p.getY) + ", " + Util.fmtDbl(p.getZ)).mkString("\n    "))
      true should be(true)

    }

    rtplanFileList.map(rtplan => testIt(rtplan))
  }
}
