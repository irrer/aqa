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

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test that the expected beams are found.
  *
  */

class TestMinCenteredFieldBeamList extends FlatSpec with Matchers {

  "beam name list" should "match expected beam name list" in {
    def testBeam(rtplanFile: File): Unit = {

      val rtplan = new DicomFile(rtplanFile).attributeList.get

      val fullBeamNameList = DicomUtil.findAllSingle(rtplan, TagByName.BeamName).map(_.getSingleStringValueOrEmptyString).distinct.map(Util.normalizeBeamName).map(_.toUpperCase())

      val qualifiedBeamNameList = Util.minCenteredFieldBeamList(rtplan, 100).map(_.toUpperCase())

      def show(beamName: String): String = {
        { if (qualifiedBeamNameList.contains(beamName)) "yes" else " no" } + " " + beamName
      }

      println(s"Beams for ${rtplanFile.getName} :\n    " + fullBeamNameList.map(show).mkString("\n    "))

      val expectedFile = (new File(rtplanFile.getParentFile, rtplanFile.getName.dropRight(4) + ".txt"))
      val expectedList = Util.readTextFile(expectedFile).right.get.split("\n").map(_.toUpperCase()).sorted

      val difference = (qualifiedBeamNameList.diff(expectedList) ++ expectedList.diff(qualifiedBeamNameList)).distinct

      if (difference.isEmpty)
        println("List of qualified beams is what it was expected to be.  Test passes.")
      else {
        Thread.sleep(500)
        println("List of wrongly qualified beams: " + difference.mkString("    "))
      }

      (difference.size) should be(0)
    }

    val testDir = new File("""src/test/resources/TestMinCenteredFieldBeamList""")
    Util.listDirFiles(testDir).filter(_.getName.toLowerCase.endsWith(".dcm")).foreach(testBeam)

  }
}
